; RUN: llc < %s -O2 -mtriple=m680x0-linux-gnu -verify-machineinstrs \
; RUN:              -code-model=medium -relocation-model=pic        \
; RUN: | FileCheck %s -check-prefix=x00

@ptr = external global i32*
@dst = external global i32
@src = external global i32

; x00-LABEL: test0:
; x00:       move.l (dst@GOTPCREL,%pc), %a0
; x00-NEXT:  move.l (ptr@GOTPCREL,%pc), %a1
; x00-NEXT:  %a0, (%a1)
; x00-NEXT:  move.l (src@GOTPCREL,%pc), %a1
; x00-NEXT:  (%a1), (%a0)
; x00-NEXT:  rts
define void @test0() nounwind {
entry:
    store i32* @dst, i32** @ptr
    %tmp.s = load i32, i32* @src
    store i32 %tmp.s, i32* @dst
    ret void
}

@ptr2 = global i32* null
@dst2 = global i32 0
@src2 = global i32 0

; x00-LABEL: test1:
; x00:       move.l (dst2@GOTPCREL,%pc), %a0
; x00-NEXT:  move.l (ptr2@GOTPCREL,%pc), %a1
; x00-NEXT:  %a0, (%a1)
; x00-NEXT:  move.l (src2@GOTPCREL,%pc), %a1
; x00-NEXT:  (%a1), (%a0)
; x00-NEXT:  rts
define void @test1() nounwind {
entry:
    store i32* @dst2, i32** @ptr2
    %tmp.s = load i32, i32* @src2
    store i32 %tmp.s, i32* @dst2
    ret void
}

declare i8* @malloc(i32)

; x00-LABEL: test2:
; x00:       move.l #40, (%sp)
; x00:  jsr (malloc@PLT,%pc)
define void @test2() nounwind {
entry:
    %ptr = call i8* @malloc(i32 40)
    ret void
}

@pfoo = external global void(...)*
declare void(...)* @afoo(...)

; x00-LABEL: test3:
; x00:       jsr (afoo@PLT,%pc)
; x00-NEXT:  move.l %d0, %a0
; x00-NEXT:  move.l (pfoo@GOTPCREL,%pc), %a1
; x00-NEXT:  move.l %a0, (%a1)
; x00-NEXT:  jsr (%a0)
define void @test3() nounwind {
entry:
    %tmp = call void(...)*(...) @afoo()
    store void(...)* %tmp, void(...)** @pfoo
    %tmp1 = load void(...)*, void(...)** @pfoo
    call void(...) %tmp1()
    ret void
}

declare void @foo(...)

; x00-LABEL: test4:
; x00:       jsr (foo@PLT,%pc)
define void @test4() nounwind {
entry:
    call void(...) @foo()
    ret void
}

@ptr6 = internal global i32* null
@dst6 = internal global i32 0
@src6 = internal global i32 0

; x00-LABEL: test5:
; x00        lea (dst6,%pc), %a0
; x00        move.l %a0, (ptr6,%pc)
; x00        move.l (src6,%pc), (%a0)
define void @test5() nounwind {
entry:
    store i32* @dst6, i32** @ptr6
    %tmp.s = load i32, i32* @src6
    store i32 %tmp.s, i32* @dst6
    ret void
}


;;; Test constant pool references.
;; FIXME no constant pool atm
;; 00: .LCPI6_0:
;; 00-LABEL: test6:
;; 00:    calll .L6$pb
;; 00: .L6$pb:
;; 00:    addl	$_GLOBAL_OFFSET_TABLE_+(.L{{.*}}-.L6$pb),
;; 00:    fldl	.LCPI6_0@GOTOFF(
;define double @test6(i32 %a.u) nounwind {
;entry:
;    %tmp = icmp eq i32 %a.u,0
;    %retval = select i1 %tmp, double 4.561230e+02, double 1.234560e+02
;    ret double %retval
;}


; Test jump table references.
;
; x00-LABEL: test7:
; x00:       move.l (4,%sp), %d0
; x00-NEXT:  add.l #-1, %d0
; x00-NEXT:  move.l %d0, %d1
; x00-NEXT:  sub.l #12, %d1
; x00-NEXT:  bhi .LBB{{.*}}_14
; x00:       lea (_GLOBAL_OFFSET_TABLE_@GOTPCREL,%pc), %a0
; x00:       lsl.l #2, %d0
; x00:       add.l #.LJTI{{.*}}_0@GOTOFF, %a1
; x00:       add.l (0,%a1,%d0), %a0
; x00-NEXT:  jmp (%a0)
;
; x00:       .LBB{{.*}}_2:
; x00-NEXT:  bra foo1@PLT
; x00:       .LBB{{.*}}_8:
; x00-NEXT:  bra foo1@PLT
; x00:       .LBB{{.*}}_14:
; x00-NEXT:  bra foo6@PLT
; x00:       .LBB{{.*}}_9:
; x00-NEXT:  bra foo2@PLT
; x00:       .LBB{{.*}}_10:
; x00-NEXT:  bra foo6@PLT
; x00:       .LBB{{.*}}_12:
; x00-NEXT:  bra foo4@PLT
; x00:       .LBB{{.*}}_3:
; x00-NEXT:  bra foo2@PLT
; x00:       .LBB{{.*}}_5:
; x00-NEXT:  bra foo3@PLT
; x00:       .LBB{{.*}}_6:
; x00-NEXT:  bra foo4@PLT
; x00:       .LBB{{.*}}_11:
; x00-NEXT:  bra foo3@PLT
; x00:       .LBB{{.*}}_4:
; x00-NEXT:  bra foo6@PLT
; x00:       .LBB{{.*}}_7:
; x00-NEXT:  bra foo5@PLT
; x00:       .LBB{{.*}}_13:
; x00-NEXT:  bra foo5@PLT
;
; x00: .LJTI{{.*}}_0:
; x00: .long .LBB{{.*}}_2@GOTOFF
; x00: .long .LBB{{.*}}_8@GOTOFF
; x00: .long .LBB{{.*}}_14@GOTOFF
; x00: .long .LBB{{.*}}_9@GOTOFF
; x00: .long .LBB{{.*}}_10@GOTOFF
; x00: .long .LBB{{.*}}_12@GOTOFF
; x00: .long .LBB{{.*}}_3@GOTOFF
; x00: .long .LBB{{.*}}_5@GOTOFF
; x00: .long .LBB{{.*}}_6@GOTOFF
; x00: .long .LBB{{.*}}_11@GOTOFF
; x00: .long .LBB{{.*}}_4@GOTOFF
; x00: .long .LBB{{.*}}_7@GOTOFF
; x00: .long .LBB{{.*}}_13@GOTOFF
define void @test7(i32 %n.u) nounwind {
entry:
    switch i32 %n.u, label %bb12 [i32 1, label %bb i32 2, label %bb6 i32 4, label %bb7 i32 5, label %bb8 i32 6, label %bb10 i32 7, label %bb1 i32 8, label %bb3 i32 9, label %bb4 i32 10, label %bb9 i32 11, label %bb2 i32 12, label %bb5 i32 13, label %bb11 ]
bb:
    tail call void(...) @foo1()
    ret void
bb1:
    tail call void(...) @foo2()
    ret void
bb2:
    tail call void(...) @foo6()
    ret void
bb3:
    tail call void(...) @foo3()
    ret void
bb4:
    tail call void(...) @foo4()
    ret void
bb5:
    tail call void(...) @foo5()
    ret void
bb6:
    tail call void(...) @foo1()
    ret void
bb7:
    tail call void(...) @foo2()
    ret void
bb8:
    tail call void(...) @foo6()
    ret void
bb9:
    tail call void(...) @foo3()
    ret void
bb10:
    tail call void(...) @foo4()
    ret void
bb11:
    tail call void(...) @foo5()
    ret void
bb12:
    tail call void(...) @foo6()
    ret void
}

declare void @foo1(...)
declare void @foo2(...)
declare void @foo6(...)
declare void @foo3(...)
declare void @foo4(...)
declare void @foo5(...)
