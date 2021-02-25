; A tiny SKI calculus evaluator.
; Copyright (C) Kamila Szewczyk, 2020.
; ------------------------------------
; A bit less braindead version of the bundled 976-byte binary,
; it doesn't leak the memory like a sieve and starts a bit faster,
; at the expense of being larger out-of-the-box (I could pack it
; with my packer to around 1,66K).

format PE GUI 4.0

entry _start

include 'win32ax.inc'

; Node in memory:

; ESI     ESI+4     ESI+8
; v       v         v
; +--------------------------+
; | left  |  right  |  type  |
; +--------------------------+

node.left  equ 0
node.right equ 4
node.type  equ 8

; SKI nodes
TYPE_S  equ 0
TYPE_K  equ 1
TYPE_I  equ 2

; alloc_node node holding two other nodes.
TYPE_BI equ 3

section '.text' code readable executable writeable
    ; Program entry point.
    ; Create the dialog, hook up the dialog procedure,
    ; enter an event loop.
    proc _start
        ; Create a heap, store it's handle in asmh.
        invoke HeapCreate, 0, 0, 0
        mov DWORD [asmh], eax
        ; Get the handle of the current module
        invoke GetModuleHandleA, 0
        ; ... and use it to create a dialog box.
        ; 1 here is the resource identifier for the form.
        invoke CreateDialogParamA, eax, 1, 0, DialogProc, 0
        ; store the dialog handle in hDlg.
        mov DWORD [hDlg], eax
        ; show the dialog.
        invoke ShowWindow, eax, SW_SHOW
        ; window message loop.
    .message_loop:
        ; fetch the next message to msg.
        invoke GetMessage, msg, 0, 0, 0
        ; GetMessage returns 0 => quit
        test eax, eax
        je .quit
        ; if the return value != -1
        inc eax
        jne .isdlg
        ; return value == -1 => an error occured.
        ; ExitProcess(1)
        push 1
        jmp .die
    .isdlg:
        ; is it a dialog message?
        invoke IsDialogMessageA, hDlg, msg
        ; nope, ignore.
        test eax, eax
        jne .message_loop
        ; Otherwise, translate and dispatch it.
        invoke TranslateMessage, msg
        invoke DispatchMessage, msg
        jmp .message_loop
    .quit:
        ; ExitProcess(0)
        push 0
    .die:
        call [ExitProcess]
    endp

    ; Dialog procedure - handling incoming messages.
    proc DialogProc
        ; Stack frame construction.
        push ebp
        mov ebp, esp
        sub esp, 16
        mov edx, DWORD [ebp+12]
        mov eax, DWORD [ebp+8]
        mov ecx, DWORD [ebp+16]
        ; handle WM_CLOSE
        cmp edx, WM_CLOSE
        je .close_handler
        ; handle WM_COMMAND
        cmp edx, WM_COMMAND
        je .command_handler
        ; don't handle everything not being WM_DESTROY
        ; (return FALSE)
        cmp edx, WM_DESTROY
        jne .no_op
        ; ... so we're handling WM_DESTROY here.
        invoke PostQuitMessage, 0
        jmp .c_exit
    .close_handler:
        ; WM_CLOSE => pass around the WM_DESTROY message.
        invoke DestroyWindow, eax
    .c_exit:
        ; common WM_DESTROY and WM_CLOSE fallthrough.
        ; return TRUE.
        xor ebx, ebx
        inc ebx
        ; the only way out is to
        jmp .die
    .command_handler:
        ; 2 is the '&Quit' button ID.
        ; If anything other has been pressed, branch.
        cmp cx, 2
        jne .not_quit
        ; Quit button pressed -> die
        invoke DestroyWindow, eax
    .no_op:
        ; a RETURN FALSE stub for lacking handlers for
        ; WM_COMMAND cases and unknown message ID's.
        xor ebx, ebx
        jmp .die
    .not_quit:
        ; '&Quit' wasn't pressed, so maybe it was '&Evaluate'?
        ; return FALSE if LOWORD(ecx) != 1
        xor ebx, ebx
        dec cx
        jne .die
        ; '&Evaluate' pressed, handle that.
        ; Get the handle to the 3rd dialog item => the expression input
        invoke GetDlgItem, eax, 3
        ; stuff it in wnd
        mov DWORD [wnd], eax
        ; get the text length to allocate approperiate amount of space on the stack
        invoke GetWindowTextLengthA, eax
        ; Save the esp
        mov ecx, esp
        ; Reserve space for the null terminator.
        ; Basically, we're constructing a buffer on the stack
        lea edx, [eax+1]
        add eax, 17
        and eax, 0xFFFFFFF0
        sub ecx, eax
        mov esp, ecx
        ; While we're at it, null-terminate it.
        mov BYTE [esp], 0
        ; Read the control data, put it in the buffer.
        mov DWORD [ebp-12], ecx
        invoke GetWindowTextA, DWORD [wnd], ecx, edx
        mov ecx, DWORD [ebp-12]
        ; Evaluate it.
        call eval
        ; Reset the control text.
        invoke SetWindowText, DWORD [wnd], eax
    .die:
        ; Pop off the VLA
        lea esp, [ebp-8]
        ; Set the correct return value.
        mov eax, ebx
        ; Balance the stack
        pop ebx
        pop esi
        pop ebp
        ret 16
    endp

    ; Calculate the size of the tree, stringified.
    ; Takes the tree in eax.
    proc str_size
        ; Preserve and clear eax, make a copy of the
        ; pointer in ebx.
        push esi ebx
        xor esi, esi
        mov ebx, eax
    .loop:
        ; if node.type == TYPE_BI, then it has two children
        cmp DWORD [ebx+node.type], TYPE_BI
        jne .quit
        ; Apparently it does.
        ; left-recurse to get the lhs size
        mov eax, DWORD [ebx+node.left]
        call str_size
        ; eax contains the lhs size, so everything left
        ; is the rhs size. loop on the right node.
        mov ebx, DWORD [ebx+node.right]
        ; add two bytes for '(' and ')'
        lea esi, [esi+eax+2]
        jmp .loop
    .quit:
        ; The node doesn't have two children - return 1
        ; (a single byte for either S, K or I)
        lea eax, [esi+1]
        pop ebx esi
        ret
    endp

    ; Stringify the tree.
    ; Take it in eax. The buffer is static and
    ; it's the callers' duty to allocate it.
    proc stringify
        ; copy the node pointer to ebx
        push ebx
        mov ebx, eax
        ; first, take the node type.
        mov edx, DWORD [eax+node.type]
        ; because no matter where we branch the buffer will be used,
        ; preload it.
        mov eax, DWORD [buf]
        ; increment the current buffer pointer stored in the variable
        ; and hold own instance, which points to one byte before
        inc eax
        mov DWORD [buf], eax
        dec eax
        ; has two children?
        cmp edx, TYPE_BI
        jne .combinator
        ; alloc_node tree starts with '('
        mov BYTE [eax], '('
        ; Recurse on the lhs and rhs
        mov eax, DWORD [ebx+node.left]
        call stringify
        mov eax, DWORD [ebx+node.right]
        call stringify
        ; increment pointer, store the ')'.
        mov eax, DWORD [buf]
        mov BYTE [eax], ')'
        inc eax
        mov DWORD [buf], eax
        dec eax
        jmp .stop
    .combinator:
        ; stringify the combinator.
        ; use the lookup string for that.
        mov dl, BYTE [ski+edx]
        ; store back the letter.
        mov BYTE [eax], dl
        ; the pointer is already incremented, so we fall thru to return
    .stop:
        pop ebx
        ret
    endp

    ; a wrapper over HeapFree, which frees the pointer in eax.
    ; XXX: inline?
    proc free
        invoke HeapFree, DWORD [asmh], 0, eax
        ret
    endp

    ; free a tree recursively
    proc free_tree
        ; preserve ebx, make a copy of eax
        push ebx
        mov ebx, eax
        ; has children?
        cmp DWORD [eax+node.type], TYPE_BI
        jne .no_children
        ; recurse over children.
        mov eax, DWORD [eax+node.left]
        call free_tree
        mov eax, DWORD [ebx+node.right]
        call free_tree
    .no_children:
        ; take the copy, restore ebx, free the parent.
        mov eax, ebx
        pop ebx
        jmp free
    endp

    ; Allocate a new tree node.
    ; takes new nodes' type in eax.
    proc alloc_node
        ; preserve eax, because it will get trashed by HeapAlloc
        push ebx
        mov ebx, eax
        ; Call HeapAlloc, alloc 4 (left) + 4 (right) + 4 (type) B.
        ; Zero the memory so we don't have to set left and right to NULL.
        invoke HeapAlloc, DWORD [asmh], HEAP_ZERO_MEMORY, 4 + 4 + 4
        ; Set the type.
        mov DWORD [eax+node.type], ebx
        pop ebx
        ret
    endp

    ; read a node from the input buffer, and return it in eax.
    proc read_node
        ; preserve ebx
        push ebx
        ; load the code pointer
        mov eax, DWORD [code]
        ; increment it, store back
        inc eax
        mov DWORD [code], eax
        dec eax
        ; load a byte
        mov al, BYTE [eax]
        ; if al>'K' then al may be 'S'
        cmp al, 'K'
        je  .read_k
        jg  .maybe_s
        ; reading a tree
        cmp al, '('
        je  .read_bitree
        ; if it's not 'I', spew out an error.
        cmp al, 'I'
        jne .parse_error
        ; build an i-node
        push TYPE_I
        pop eax
        jmp .build_node
    .maybe_s:
        ; if it's not 'S', spew out an error.
        cmp al, 'S'
        jne .parse_error
        ; otherwise, clear eax (load TYPE_S)
        ; and build a new node.
        xor eax, eax
        jmp .build_node
    .read_bitree:
        ; load the approperiate type and allocate a node
        push TYPE_BI
        pop eax
        call alloc_node
        mov ebx, eax
        ; read the left node
        call read_node
        mov DWORD [ebx+node.left], eax
        ; eax = 0 => return NULL to broadcast an error.
        test eax, eax
        je .nullify
        ; read the right node
        call read_node
        mov DWORD [ebx+node.right], eax
        test eax, eax
        je .nullify
        ; no errors - increment the code pointer to skip the trailing `)`.
        inc DWORD [code]
        jmp .die
    .read_k:
        ; set eax to 1 (loading TYPE_K)
        ; and fall thru to construction of a new node.
        xor eax, eax
        inc eax
    .build_node:
        pop ebx
        jmp alloc_node
    .parse_error:
        ; in case of a parse error, display a message and fall thru to returning NULL.
        invoke MessageBoxA, 0, msge, 0, MB_OK
    .nullify:
        xor ebx, ebx
    .die:
        ; set the return value and quit
        mov eax, ebx
        pop ebx
        ret
    endp

    ; duplicate a tree in eax.
    proc dup_tree
        push esi ebx
        mov ebx, eax
        ; Make a new node with this node's type.
        mov eax, DWORD [eax+node.type]
        call alloc_node
        ; if type != TYPE_BI then return that instance.
        cmp DWORD [ebx+node.type], TYPE_BI
        jne .shallow
        ; else, clone recursively. copy the original
        ; ptr, because it will get overwritten
        mov esi, eax
        ; clone the left node
        mov eax, DWORD [ebx+node.left]
        call dup_tree
        mov DWORD [esi+node.left], eax
        ; clone the right node
        mov eax, DWORD [ebx+node.right]
        call dup_tree
        mov DWORD [esi+node.right], eax
        ; restore eax
        mov eax, esi
    .shallow:
        pop ebx esi
        ret
    endp

    proc eval_step
        push edi esi ebx
        mov ebx, eax
        ; has one child? if node.left == NULL
        mov eax, DWORD [eax+node.left]
        test eax, eax
        je .no_left
        ; if the first child's type is I
        cmp DWORD [eax+node.type], TYPE_I
        jne .not_inode
        ; got identity, so take the right node.
        mov esi, DWORD [ebx+node.right]
        ; free this node and the left node.
        jmp .clean
    .not_inode:
        ; it's not I. eax is now orig->left
        ; if orig->left->left->type == K
        mov edx, DWORD [eax+node.left]
        ; wait, maybe there is no left node
        test edx, edx
        je .no_left
        ; check the type.
        cmp DWORD [edx+node.type], TYPE_K
        ; branch if it's not K either.
        jne .not_knode
        ; free orig->right and orig->left->left
        ; keep and return orig->left->right
        mov esi, DWORD [eax+node.right]
        mov eax, DWORD [ebx+node.right]
        call free_tree
        mov eax, DWORD [ebx+node.left]
        mov eax, DWORD [eax+node.left]
        ; fallthru to free the orig->left->left node
    .clean:
        call free_tree
        mov eax, ebx
        call free
    .yield_saved:
        mov ebx, esi
        jmp .done
    .not_knode:
        ; if it's not a K or I node, then for sure it's either
        ; a node we have to evaluate recursively _or_ a S node.
        ; check for existence of X = orig->left->left->left
        mov edx, DWORD [edx]
        test edx, edx
        je .no_left
        ; X->type != TYPE_S?
        cmp DWORD [edx+node.type], TYPE_S
        jne .no_left
        ; ok, so definitely it's a S node.
        ; to get ((Sx)y)z = ((xz)(yz)), first build the outer binode.
        push TYPE_BI
        pop eax
        call alloc_node
        ; OK, save it in esi
        mov esi, eax
        ; build two another binodes, and put them as the left and right
        ; node of this tree.
        push 3
        pop eax
        call alloc_node
        mov DWORD [esi+node.left], eax
        push 3
        pop eax
        call alloc_node
        mov DWORD [esi+node.right], eax
        ; now the magic happens. do the following:
        ; (esi + node.left)->left = dup(orig->left->left->right)
        ; (esi + node.left)->right = dup(orig->right)
        ; (esi + node.right)->left = dup(orig->left->right)
        ; (esi + node.right)->right = dup(orig->right)
        ; I'm not sure if this many dup calls are required, but they
        ; help to shave off some space and trouble needed to free the
        ; correct elements of the trees. we're not really aiming for
        ; performance here, so it's alright.
        mov edi, DWORD [esi+node.left]
        mov eax, DWORD [ebx+node.left]
        mov eax, DWORD [eax+node.left]
        mov eax, DWORD [eax+node.right]
        call dup_tree
        mov DWORD [edi+node.left], eax
        mov eax, DWORD [ebx+node.right]
        mov edi, DWORD [esi+node.left]
        call dup_tree
        mov DWORD [edi+node.right], eax
        mov eax, DWORD [ebx+node.left]
        mov edi, DWORD [esi+node.right]
        mov eax, DWORD [eax+node.right]
        call dup_tree
        mov DWORD [edi+node.left], eax
        mov eax, DWORD [ebx+node.right]
        mov edi, DWORD [esi+node.right]
        call dup_tree
        mov DWORD [edi+node.right], eax
        ; free the original tree
        mov eax, ebx
        call free_tree
        jmp .yield_saved
    .no_left:
        ; maybe it's a binode, which we just need to evaluate
        ; deeper to get some observable result?
        cmp DWORD [ebx+node.type], TYPE_BI
        jne .done
        ; recurse twice. first set the left node, then the right node.
        call eval_step
        mov DWORD [ebx+node.left], eax
        mov eax, DWORD [ebx+node.right]
        call eval_step
        mov DWORD [ebx+node.right], eax
    .done:
        mov eax, ebx
        pop ebx esi edi
        ret
    endp

    ; the evaluation wrapper called by the DialogProc.
    ; takes the input buffer in ecx.
    eval:
        push esi ebx
        mov ebx, ecx
        ; store the input in the code buffer.
        mov DWORD [code], ecx
        ; read the expression.
        call read_node
        ; if read_node returns null, then an error occured
        test eax, eax
        je .read_fail
        ; call the evaluation procedure.
        call eval_step
        mov esi, eax
        ; find out the size of the buffer, stringified.
        call str_size
        ; allocate it a byte of extra space.
        inc eax
        invoke HeapAlloc, DWORD [asmh], 0, eax
        ; initialize the output buffer.
        mov DWORD [buf], eax
        ; save the output copy to ourselves to later return it.
        mov ebx, eax
        ; take back the saved buffer, stringify the input into it
        mov eax, esi
        call stringify
        ; NULL terminate
        mov eax, DWORD [buf]
        mov BYTE [eax], 0
        ; free the original tree
        mov eax, esi
        call free_tree
    .read_fail:
        ; in any case, return the value we've got.
        mov eax, ebx
        pop ebx esi
        ret

wnd:  dd 0
msg   MSG
hDlg: dd 0
asmh: dd 0
buf:  dd 0
code: dd 0
ski:  db 'SKI', 0
msge: db '?', 0

section '.rsrc' resource data readable
directory RT_DIALOG, dialogs
resource dialogs, 1, LANG_ENGLISH+SUBLANG_DEFAULT, demo
dialog demo,'SKI calculus',70,70,330,20,WS_CAPTION+WS_POPUP+WS_SYSMENU+DS_MODALFRAME
    dialogitem 'STATIC', '&Code: ', 4, 4, 5, 21, 9, WS_VISIBLE+WS_CHILD+WS_GROUP
    dialogitem 'BUTTON', '&Quit', 2, 269, 4, 50, 11, BS_PUSHBUTTON+WS_CHILD+WS_VISIBLE+WS_GROUP
    dialogitem 'BUTTON', '&Evaluate', 1, 218, 4, 50, 11, BS_DEFPUSHBUTTON+WS_CHILD+WS_VISIBLE+WS_GROUP
    dialogitem 'EDIT', '', 3, 28, 3, 187, 14, ES_LEFT+WS_CHILD+WS_VISIBLE+WS_BORDER+WS_GROUP+ES_AUTOHSCROLL
enddialog

section '.idata' import data readable writable
library kernel32, 'KERNEL32.DLL', \
        user32, 'USER32.DLL'

include 'api\kernel32.inc'
include 'api\user32.inc'
