; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple=jvm32-unknown-unknown | FileCheck %s --check-prefix=JVM -match-full-lines


; Function Attrs: noinline nounwind uwtable
define dso_local i32 @all_loops(i32 %init) #0 {
; JVM:       iload 0
; JVM-NEXT:    istore 1
; JVM-NEXT:    ldc 0
; JVM-NEXT:    istore 2
; JVM-NEXT:  LBB0_1:
; JVM-NEXT:    iload 2
; JVM-NEXT:    istore 3
; JVM-NEXT:    iload 1
; JVM-NEXT:    istore 4
; JVM-NEXT:    iload 3
; JVM-NEXT:    ldc 9
; JVM-NEXT:    iload 4
; JVM-NEXT:    istore 5
; JVM-NEXT:    if_icmpgt LBB0_4
; JVM-NEXT:    goto LBB0_2
; JVM-NEXT:  LBB0_2:
; JVM-NEXT:    iload 4
; JVM-NEXT:    iload 3
; JVM-NEXT:    iadd
; JVM-NEXT:    istore 25
; JVM-NEXT:    iload 3
; JVM-NEXT:    iconst_1
; JVM-NEXT:    iadd
; JVM-NEXT:    istore 26
; JVM-NEXT:    iload 25
; JVM-NEXT:    istore 1
; JVM-NEXT:    iload 26
; JVM-NEXT:    istore 2
; JVM-NEXT:    goto LBB0_1
; JVM-NEXT:  LBB0_4:
; JVM-NEXT:    iload 5
; JVM-NEXT:    istore 6
; JVM-NEXT:    iload 6
; JVM-NEXT:    istore 7
; JVM-NEXT:    ldc 10
; JVM-NEXT:    istore 8
; JVM-NEXT:  LBB0_5:
; JVM-NEXT:    iload 8
; JVM-NEXT:    istore 9
; JVM-NEXT:    iload 7
; JVM-NEXT:    istore 10
; JVM-NEXT:    iload 9
; JVM-NEXT:    iconst_1
; JVM-NEXT:    iload 10
; JVM-NEXT:    istore 11
; JVM-NEXT:    iload 9
; JVM-NEXT:    istore 12
; JVM-NEXT:    if_icmplt LBB0_7
; JVM-NEXT:    goto LBB0_6
; JVM-NEXT:  LBB0_6:
; JVM-NEXT:    iload 9
; JVM-NEXT:    ldc -1
; JVM-NEXT:    iadd
; JVM-NEXT:    istore 23
; JVM-NEXT:    iload 10
; JVM-NEXT:    ldc -1
; JVM-NEXT:    iadd
; JVM-NEXT:    istore 24
; JVM-NEXT:    iload 24
; JVM-NEXT:    istore 7
; JVM-NEXT:    iload 23
; JVM-NEXT:    istore 8
; JVM-NEXT:    goto LBB0_5
; JVM-NEXT:  LBB0_7:
; JVM-NEXT:    iload 12
; JVM-NEXT:    istore 13
; JVM-NEXT:    iload 11
; JVM-NEXT:    istore 14
; JVM-NEXT:    iload 14
; JVM-NEXT:    istore 15
; JVM-NEXT:    iload 13
; JVM-NEXT:    istore 16
; JVM-NEXT:  LBB0_8:
; JVM-NEXT:    iload 16
; JVM-NEXT:    istore 17
; JVM-NEXT:    iload 15
; JVM-NEXT:    istore 18
; JVM-NEXT:    iload 17
; JVM-NEXT:    iconst_1
; JVM-NEXT:    iadd
; JVM-NEXT:    istore 19
; JVM-NEXT:    iload 18
; JVM-NEXT:    ldc -1
; JVM-NEXT:    iadd
; JVM-NEXT:    istore 20
; JVM-NEXT:    iload 19
; JVM-NEXT:    ldc 10
; JVM-NEXT:    iload 20
; JVM-NEXT:    istore 15
; JVM-NEXT:    iload 19
; JVM-NEXT:    istore 16
; JVM-NEXT:    iload 20
; JVM-NEXT:    istore 21
; JVM-NEXT:    if_icmplt LBB0_8
; JVM-NEXT:    goto LBB0_10
; JVM-NEXT:  LBB0_10:
; JVM-NEXT:    iload 21
; JVM-NEXT:    istore 22
; JVM-NEXT:    iload 22
; JVM-NEXT:    ireturn
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %csum.0 = phi i32 [ %init, %entry ], [ %add, %for.inc ]
  %i.0 = phi i32 [ 0, %entry ], [ %inc, %for.inc ]
  %cmp = icmp slt i32 %i.0, 10
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %add = add nsw i32 %csum.0, %i.0
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %inc = add nsw i32 %i.0, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  br label %while.cond

while.cond:                                       ; preds = %while.body, %for.end
  %csum.1 = phi i32 [ %csum.0, %for.end ], [ %sub, %while.body ]
  %i.1 = phi i32 [ 10, %for.end ], [ %dec, %while.body ]
  %cmp1 = icmp sgt i32 %i.1, 0
  br i1 %cmp1, label %while.body, label %while.end

while.body:                                       ; preds = %while.cond
  %sub = sub nsw i32 %csum.1, 1
  %dec = add nsw i32 %i.1, -1
  br label %while.cond

while.end:                                        ; preds = %while.cond
  br label %do.body

do.body:                                          ; preds = %do.cond, %while.end
  %csum.2 = phi i32 [ %csum.1, %while.end ], [ %dec2, %do.cond ]
  %i.2 = phi i32 [ %i.1, %while.end ], [ %inc3, %do.cond ]
  %dec2 = add nsw i32 %csum.2, -1
  %inc3 = add nsw i32 %i.2, 1
  br label %do.cond

do.cond:                                          ; preds = %do.body
  %cmp4 = icmp slt i32 %inc3, 10
  br i1 %cmp4, label %do.body, label %do.end

do.end:                                           ; preds = %do.cond
  ret i32 %dec2
}

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @all_conditions(i32 %cond, i32 %a, i32 %b, i32 %c, i32 %d) #0 {
; JVM:       iconst_0
; JVM-NEXT:    ifne LBB1_4
; JVM-NEXT:    goto LBB1_3
; JVM-NEXT:  LBB1_3:
; JVM-NEXT:    iload 0
; JVM-NEXT:    iload 1
; JVM-NEXT:    imul
; JVM-NEXT:    istore 7
; JVM-NEXT:    iload 7
; JVM-NEXT:    istore 5
; JVM-NEXT:    goto LBB1_5
; JVM-NEXT:  LBB1_4:
; JVM-NEXT:    iload 2
; JVM-NEXT:    iload 3
; JVM-NEXT:    isub
; JVM-NEXT:    istore 4
; JVM-NEXT:    iload 4
; JVM-NEXT:    istore 5
; JVM-NEXT:  LBB1_5:
; JVM-NEXT:    iload 5
; JVM-NEXT:    istore 6
; JVM-NEXT:    iload 6
; JVM-NEXT:    ireturn
entry:
  switch i32 %cond, label %sw.default [
    i32 1, label %sw.bb
    i32 2, label %sw.bb1
  ]

sw.bb:                                            ; preds = %entry
  br label %sw.bb1

sw.bb1:                                           ; preds = %sw.bb, %entry
  br label %sw.default

sw.default:                                       ; preds = %sw.bb1, %entry
  br label %sw.epilog

sw.epilog:                                        ; preds = %sw.default
  %tobool = icmp ne i32 3, 0
  br i1 %tobool, label %if.then, label %if.else

if.then:                                          ; preds = %sw.epilog
  %mul = mul nsw i32 %a, %b
  br label %if.end

if.else:                                          ; preds = %sw.epilog
  %sub = sub nsw i32 %c, %d
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  %if_val.0 = phi i32 [ %mul, %if.then ], [ %sub, %if.else ]
  ret i32 %if_val.0
}
