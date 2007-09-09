;
; TinyPTC x11 v0.7.3 MMX-Optimized YV12 converter
; Copyright (C) 2002 Fred Howell <foohoo@shaw.ca>
;
; http://www.sourceforge.net/projects/tinyptc/
;
; This library is free software; you can redistribute it and/or
; modify it under the terms of the GNU Lesser General Public
; License as published by the Free Software Foundation; either
; version 2 of the License, or (at your option) any later version.
;
; This library is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
; Lesser General Public License for more details.
;
; You should have received a copy of the GNU Lesser General Public
; License along with this library; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;

bits 32

global convert_yv12_mmx

section .data

align 16

;	static short ygr0bcoff[] = {33058/2,16390/2,0,6405/2};
;	static short vgr0bcoff[] = {-24110/2,28781/2,0,-4671/2};
;	static short ugr0bcoff[] = {-19068/2,-9713/2,0,28781/2};
;	static short yb0grcoff[] = {6405/2,0,33058/2,16390/2};
;	static short vb0grcoff[] = {-4671/2,0,-24110/2,28781/2};
;	static short ub0grcoff[] = {28781/2,-19068/2,-9713/2,0};
;
;	static unsigned short add2w[] = {1,1,1,1};
;	static short aoff[] = {16,128,16,128};
;	static unsigned char bmask[] = {0xff, 0, 0, 0, 0, 0, 0, 0};
;	static unsigned short grmask[] = {0,0xffff,0,0};

thezero dw  0,0,0,0
ygr0bcoff dw 0x4091,0x2003,0x0000,0x0c82
ugr0bcoff dw 0xdac2,0xed08,0x0000,0x3836
vgr0bcoff dw 0xd0e9,0x3836,0x0000,0xf6e1
yb0grcoff dw 0x0c82,0x0000,0x4091,0x2003
ub0grcoff dw 0x3836,0x0000,0xdac2,0xed08
vb0grcoff dw 0xf6e1,0x0000,0xd0e9,0x3836
yoff  dw 0x0010,0x0010,0x0010,0x0010
uvoff  dw 0x0080,0x0080,0x0080,0x0080
add2w dw 1,1,1,1
grmask dw  0,0xffff,0,0
bmask  db 0xff,0,0,0, 0,0,0,0


section text

align 16

convert_yv12_mmx:

push ebp
push eax
push ebx
push ecx
push edx
push edi
push esi

;// initialisation du mm7 à zero
pxor mm7,mm7

%assign _P 7*4
mov edx, [esp+_P+ 4]
mov ebx, [esp+_P+8]
mov ebp,[esp+_P+12]
mov eax,[esp+_P+16]
mov ecx,[esp+_P+20]
mov edi,ecx
mov esi,ecx
shr ecx, 3
shl edi, 2


.while:
; 1ere quad 1ere ligne
	movq mm3, [edx];
	movq mm1,mm3;
	psrlq mm3,8;
	movq mm4,mm1;
	psrlq mm1, 24;
	pand mm4, [bmask];
	pand mm1, [grmask];
	por  mm4, mm1;
	punpcklbw mm3, [thezero];
	punpcklbw mm4, [thezero];

; Y
	movq mm5, mm3;
			
	pmaddwd mm5, [ygr0bcoff];

	movq mm1, mm4;

	pmaddwd mm1, [yb0grcoff];

	paddd mm5, mm1
	psrad mm5,15;
			

; U
	movq mm6, mm3;
			
	pmaddwd mm6, [ugr0bcoff];

	movq mm1, mm4;

	pmaddwd mm1, [ub0grcoff];

	paddd mm6, mm1;
	psrad mm6,15;


; V
	
	pmaddwd mm3, [vgr0bcoff];


	pmaddwd mm4, [vb0grcoff];

	paddd mm3, mm4;
	psrad mm3,15;

	movq mm7, mm3;

; 1ere quad 2eme ligne
	movq mm3, [edx+edi];
	movq mm1,mm3;
	psrlq mm3,8;
	movq mm4,mm1;
	psrlq mm1, 24;
	pand mm4, [bmask];
	pand mm1, [grmask];
	por  mm4, mm1;
	punpcklbw mm3, [thezero];
	punpcklbw mm4, [thezero];


; Y
	movq mm2, mm3;
			
	pmaddwd mm2, [ygr0bcoff];

	movq mm1, mm4;

	pmaddwd mm1, [yb0grcoff];

	paddd mm2, mm1
	psrad mm2,15;

; U
	movq mm0, mm3;
			
	pmaddwd mm0, [ugr0bcoff];

	movq mm1, mm4;

	pmaddwd mm1, [ub0grcoff];

	paddd mm0, mm1;
	psrad mm0,15;

	packssdw mm6,mm0;
	pmaddwd mm6,[add2w];

	packssdw mm6,[thezero];
; V
	
	pmaddwd mm3, [vgr0bcoff];


	pmaddwd mm4, [vb0grcoff];

	paddd mm3, mm4;
	psrad mm3,15;

	packssdw mm7,mm3;
	pmaddwd mm7,[add2w];

	packssdw mm7,[thezero];

; 2eme quad 1ere ligne
	movq mm3, [edx+8];
	movq mm1,mm3;
	psrlq mm3,8;
	movq mm4,mm1;
	psrlq mm1, 24;
	pand mm4, [bmask];
	pand mm1, [grmask];
	por  mm4, mm1;
	punpcklbw mm3, [thezero];
	punpcklbw mm4, [thezero];

; Y
	movq mm0, mm3;
			
	pmaddwd mm0, [ygr0bcoff];

	movq mm1, mm4;

	pmaddwd mm1, [yb0grcoff];

	paddd mm0, mm1
	psrad mm0,15;
	packssdw mm5,mm0
			
	paddw mm5,[yoff]
	packuswb mm5,mm5

	movd [ebx], mm5

; U
	movq mm0, mm3;
			
	pmaddwd mm0, [ugr0bcoff];

	movq mm1, mm4;

	pmaddwd mm1, [ub0grcoff];

	paddd mm0, mm1;
	psrad mm0,15;

	packssdw mm0,mm0
	psllq mm0,32

	paddw mm6,mm0


; V
	
	pmaddwd mm3, [vgr0bcoff];


	pmaddwd mm4, [vb0grcoff];

	paddd mm3, mm4;
	psrad mm3,15;

	packssdw mm3,mm3
	psllq mm3,32

	paddw mm7,mm3

; 2eme quad 2eme ligne
	movq mm3, [edx+edi+8];
	movq mm1,mm3;
	psrlq mm3,8;
	movq mm4,mm1;
	psrlq mm1, 24;
	pand mm4, [bmask];
	pand mm1, [grmask];
	por  mm4, mm1;
	punpcklbw mm3, [thezero];
	punpcklbw mm4, [thezero];


; Y
	movq mm0, mm3;
			
	pmaddwd mm0, [ygr0bcoff];

	movq mm1, mm4;

	pmaddwd mm1, [yb0grcoff];

	paddd mm0, mm1
	psrad mm0,15;
	packssdw mm2,mm0

	paddw mm2,[yoff]
	packuswb mm2,mm2

	movd [ebx+esi], mm2

; U
	movq mm0, mm3;
			
	pmaddwd mm0, [ugr0bcoff];

	movq mm1, mm4;

	pmaddwd mm1, [ub0grcoff];

	paddd mm0, mm1;
	psrad mm0,15;

	packssdw mm0,mm0
	psllq mm0,32

	paddw mm6,mm0
	pmaddwd mm6,[add2w]
	packssdw mm6,[thezero]
; V
	
	pmaddwd mm3, [vgr0bcoff];


	pmaddwd mm4, [vb0grcoff];

	paddd mm3, mm4;
	psrad mm3,15;

	packssdw mm3,mm3
	psllq mm3,32

	paddw mm7,mm3
	pmaddwd mm7,[add2w]
	packssdw mm7,[thezero]

; 3eme quad 1ere ligne
	movq mm3, [edx+16];
	movq mm1,mm3;
	psrlq mm3,8;
	movq mm4,mm1;
	psrlq mm1, 24;
	pand mm4, [bmask];
	pand mm1, [grmask];
	por  mm4, mm1;
	punpcklbw mm3, [thezero];
	punpcklbw mm4, [thezero];

; Y
	movq mm5, mm3;
			
	pmaddwd mm5, [ygr0bcoff];

	movq mm1, mm4;

	pmaddwd mm1, [yb0grcoff];

	paddd mm5, mm1
	psrad mm5,15;
			

; U
	movq mm0, mm3;
			
	pmaddwd mm0, [ugr0bcoff];

	movq mm1, mm4;

	pmaddwd mm1, [ub0grcoff];

	paddd mm0, mm1;
	psrad mm0,15;

	packssdw mm0,mm0
	pmaddwd mm0,[add2w]
	psllq mm0,32
	paddw mm6, mm0

; V
	
	pmaddwd mm3, [vgr0bcoff];


	pmaddwd mm4, [vb0grcoff];

	paddd mm3, mm4;
	psrad mm3,15;

	packssdw mm3,mm3
	pmaddwd mm3,[add2w]
	psllq mm3,32
	paddw mm7, mm3

; 3eme quad 2eme ligne
	movq mm3, [edx+edi+16];
	movq mm1,mm3;
	psrlq mm3,8;
	movq mm4,mm1;
	psrlq mm1, 24;
	pand mm4, [bmask];
	pand mm1, [grmask];
	por  mm4, mm1;
	punpcklbw mm3, [thezero];
	punpcklbw mm4, [thezero];


; Y
	movq mm2, mm3;
			
	pmaddwd mm2, [ygr0bcoff];

	movq mm1, mm4;

	pmaddwd mm1, [yb0grcoff];

	paddd mm2, mm1
	psrad mm2,15;

; U
	movq mm0, mm3;
			
	pmaddwd mm0, [ugr0bcoff];

	movq mm1, mm4;

	pmaddwd mm1, [ub0grcoff];

	paddd mm0, mm1;
	psrad mm0,15;

	packssdw mm0,mm0
	pmaddwd mm0,[add2w]
	psllq mm0,32
	paddw mm6, mm0
; V
	
	pmaddwd mm3, [vgr0bcoff];


	pmaddwd mm4, [vb0grcoff];

	paddd mm3, mm4;
	psrad mm3,15;

	packssdw mm3,mm3
	pmaddwd mm3,[add2w]
	psllq mm3,32
	paddw mm7, mm3

; 4eme quad 1ere ligne
	movq mm3, [edx+24];
	movq mm1,mm3;
	psrlq mm3,8;
	movq mm4,mm1;
	psrlq mm1, 24;
	pand mm4, [bmask];
	pand mm1, [grmask];
	por  mm4, mm1;
	punpcklbw mm3, [thezero];
	punpcklbw mm4, [thezero];

; Y
	movq mm0, mm3;
			
	pmaddwd mm0, [ygr0bcoff];

	movq mm1, mm4;

	pmaddwd mm1, [yb0grcoff];

	paddd mm0, mm1
	psrad mm0,15;
	packssdw mm5,mm0
			
	paddw mm5,[yoff]
	packuswb mm5,mm5

	movd [ebx+4], mm5

; U
	movq mm0, mm3;
			
	pmaddwd mm0, [ugr0bcoff];

	movq mm1, mm4;

	pmaddwd mm1, [ub0grcoff];

	paddd mm0, mm1;
	psrad mm0,15;

	packssdw mm0,mm0
	pmaddwd mm0,[add2w]
	psllq mm0,48
	paddw mm6, mm0

; V
	
	pmaddwd mm3, [vgr0bcoff];


	pmaddwd mm4, [vb0grcoff];

	paddd mm3, mm4;
	psrad mm3,15;

	packssdw mm3,mm3
	pmaddwd mm3,[add2w]
	psllq mm3,48
	paddw mm7, mm3

; 4eme quad 2eme line
	movq mm3, [edx+edi+24];
	movq mm1,mm3;
	psrlq mm3,8;
	movq mm4,mm1;
	psrlq mm1, 24;
	pand mm4, [bmask];
	pand mm1, [grmask];
	por  mm4, mm1;
	punpcklbw mm3, [thezero];
	punpcklbw mm4, [thezero];


; Y
	movq mm0, mm3;
			
	pmaddwd mm0, [ygr0bcoff];

	movq mm1, mm4;

	pmaddwd mm1, [yb0grcoff];

	paddd mm0, mm1
	psrad mm0,15;
	packssdw mm2,mm0

	paddw mm2,[yoff]
	packuswb mm2,mm2

	movd [ebx+esi+4], mm2

; U
	movq mm0, mm3;
			
	pmaddwd mm0, [ugr0bcoff];

	movq mm1, mm4;

	pmaddwd mm1, [ub0grcoff];

	paddd mm0, mm1;
	psrad mm0,15;

	packssdw mm0,mm0
	pmaddwd mm0,[add2w]
	psllq mm0,48
	paddw mm6, mm0

	psraw mm6,2
	paddw mm6, [uvoff]
	packuswb mm6,mm6
	movd [eax],mm6
; V
	
	pmaddwd mm3, [vgr0bcoff];


	pmaddwd mm4, [vb0grcoff];

	paddd mm3, mm4;
	psrad mm3,15;

	packssdw mm3,mm3
	pmaddwd mm3,[add2w]
	psllq mm3,48
	paddw mm7, mm3

	psraw mm7,2
	paddw mm7, [uvoff]
	packuswb mm7,mm7
	movd [ebp],mm7


	dec ecx

	cmp ecx,0

jz .fin_while

; preparations pour les 4 quads suivantes
	lea edx, [edx + 32];
	lea ebx, [ebx + 8];
	lea eax, [eax + 4];
	lea ebp, [ebp + 4];
	
jmp .while

.fin_while:
emms

pop esi
pop edi
pop edx
pop ecx
pop ebx
pop eax
pop ebp

ret                  ;//The End
