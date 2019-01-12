; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py

; RUN: llc < %s -mtriple=jvm32-unknown-unknown | FileCheck %s --check-prefix=JVM -match-full-lines

; Function Attrs: noinline nounwind uwtable
define  i32 @LT(i64 %l1, i64 %l2) #0 {
; JVM:       lload 0
; JVM-NEXT:    lload 2
; JVM-NEXT:    lcmp
; JVM-NEXT:    iconst_0
; JVM-NEXT:    if_icmpge LBB0_2
; JVM-NEXT:    goto LBB0_1
; JVM-NEXT:  LBB0_1:
; JVM-NEXT:    ldc 1
; JVM-NEXT:    istore 5
; JVM-NEXT:    goto LBB0_3
; JVM-NEXT:  LBB0_2:
; JVM-NEXT:    ldc 0
; JVM-NEXT:    istore 5
; JVM-NEXT:  LBB0_3:
; JVM-NEXT:    iload 5
; JVM-NEXT:    istore 6
; JVM-NEXT:    iload 6
; JVM-NEXT:    ireturn
entry:
  %cmp = icmp slt i64 %l1, %l2
  br i1 %cmp, label %if.then, label %if.end

if.then:                                          ; preds = %entry
  br label %return

if.end:                                           ; preds = %entry
  br label %return

return:                                           ; preds = %if.end, %if.then
  %retval.0 = phi i32 [ 1, %if.then ], [ 0, %if.end ]
  ret i32 %retval.0
}

; Function Attrs: noinline nounwind uwtable
define  i32 @LE(i64 %l1, i64 %l2) #0 {
; JVM:       lload 0
; JVM-NEXT:    lload 2
; JVM-NEXT:    lcmp
; JVM-NEXT:    iconst_0
; JVM-NEXT:    if_icmpgt LBB1_2
; JVM-NEXT:    goto LBB1_1
; JVM-NEXT:  LBB1_1:
; JVM-NEXT:    ldc 1
; JVM-NEXT:    istore 5
; JVM-NEXT:    goto LBB1_3
; JVM-NEXT:  LBB1_2:
; JVM-NEXT:    ldc 0
; JVM-NEXT:    istore 5
; JVM-NEXT:  LBB1_3:
; JVM-NEXT:    iload 5
; JVM-NEXT:    istore 6
; JVM-NEXT:    iload 6
; JVM-NEXT:    ireturn
entry:
  %cmp = icmp sle i64 %l1, %l2
  br i1 %cmp, label %if.then, label %if.end

if.then:                                          ; preds = %entry
  br label %return

if.end:                                           ; preds = %entry
  br label %return

return:                                           ; preds = %if.end, %if.then
  %retval.0 = phi i32 [ 1, %if.then ], [ 0, %if.end ]
  ret i32 %retval.0
}

; Function Attrs: noinline nounwind uwtable
define  i32 @GT(i64 %l1, i64 %l2) #0 {
; JVM:       lload 0
; JVM-NEXT:    lload 2
; JVM-NEXT:    lcmp
; JVM-NEXT:    iconst_0
; JVM-NEXT:    if_icmple LBB2_2
; JVM-NEXT:    goto LBB2_1
; JVM-NEXT:  LBB2_1:
; JVM-NEXT:    ldc 1
; JVM-NEXT:    istore 5
; JVM-NEXT:    goto LBB2_3
; JVM-NEXT:  LBB2_2:
; JVM-NEXT:    ldc 0
; JVM-NEXT:    istore 5
; JVM-NEXT:  LBB2_3:
; JVM-NEXT:    iload 5
; JVM-NEXT:    istore 6
; JVM-NEXT:    iload 6
; JVM-NEXT:    ireturn
entry:
  %cmp = icmp sgt i64 %l1, %l2
  br i1 %cmp, label %if.then, label %if.end

if.then:                                          ; preds = %entry
  br label %return

if.end:                                           ; preds = %entry
  br label %return

return:                                           ; preds = %if.end, %if.then
  %retval.0 = phi i32 [ 1, %if.then ], [ 0, %if.end ]
  ret i32 %retval.0
}

; Function Attrs: noinline nounwind uwtable
define  i32 @GE(i64 %l1, i64 %l2) #0 {
; JVM:       lload 0
; JVM-NEXT:    lload 2
; JVM-NEXT:    lcmp
; JVM-NEXT:    iconst_0
; JVM-NEXT:    if_icmplt LBB3_2
; JVM-NEXT:    goto LBB3_1
; JVM-NEXT:  LBB3_1:
; JVM-NEXT:    ldc 1
; JVM-NEXT:    istore 5
; JVM-NEXT:    goto LBB3_3
; JVM-NEXT:  LBB3_2:
; JVM-NEXT:    ldc 0
; JVM-NEXT:    istore 5
; JVM-NEXT:  LBB3_3:
; JVM-NEXT:    iload 5
; JVM-NEXT:    istore 6
; JVM-NEXT:    iload 6
; JVM-NEXT:    ireturn
entry:
  %cmp = icmp sge i64 %l1, %l2
  br i1 %cmp, label %if.then, label %if.end

if.then:                                          ; preds = %entry
  br label %return

if.end:                                           ; preds = %entry
  br label %return

return:                                           ; preds = %if.end, %if.then
  %retval.0 = phi i32 [ 1, %if.then ], [ 0, %if.end ]
  ret i32 %retval.0
}

; Function Attrs: noinline nounwind uwtable
define  i32 @EQ(i64 %l1, i64 %l2) #0 {
; JVM:       lload 0
; JVM-NEXT:    lload 2
; JVM-NEXT:    lcmp
; JVM-NEXT:    iconst_0
; JVM-NEXT:    if_icmpne LBB4_2
; JVM-NEXT:    goto LBB4_1
; JVM-NEXT:  LBB4_1:
; JVM-NEXT:    ldc 1
; JVM-NEXT:    istore 5
; JVM-NEXT:    goto LBB4_3
; JVM-NEXT:  LBB4_2:
; JVM-NEXT:    ldc 0
; JVM-NEXT:    istore 5
; JVM-NEXT:  LBB4_3:
; JVM-NEXT:    iload 5
; JVM-NEXT:    istore 6
; JVM-NEXT:    iload 6
; JVM-NEXT:    ireturn
entry:
  %cmp = icmp eq i64 %l1, %l2
  br i1 %cmp, label %if.then, label %if.end

if.then:                                          ; preds = %entry
  br label %return

if.end:                                           ; preds = %entry
  br label %return

return:                                           ; preds = %if.end, %if.then
  %retval.0 = phi i32 [ 1, %if.then ], [ 0, %if.end ]
  ret i32 %retval.0
}

; Function Attrs: noinline nounwind uwtable
define  i32 @NE(i64 %l1, i64 %l2) #0 {
; JVM:       lload 0
; JVM-NEXT:    lload 2
; JVM-NEXT:    lcmp
; JVM-NEXT:    iconst_0
; JVM-NEXT:    if_icmpeq LBB5_2
; JVM-NEXT:    goto LBB5_1
; JVM-NEXT:  LBB5_1:
; JVM-NEXT:    ldc 1
; JVM-NEXT:    istore 5
; JVM-NEXT:    goto LBB5_3
; JVM-NEXT:  LBB5_2:
; JVM-NEXT:    ldc 0
; JVM-NEXT:    istore 5
; JVM-NEXT:  LBB5_3:
; JVM-NEXT:    iload 5
; JVM-NEXT:    istore 6
; JVM-NEXT:    iload 6
; JVM-NEXT:    ireturn
entry:
  %cmp = icmp ne i64 %l1, %l2
  br i1 %cmp, label %if.then, label %if.end

if.then:                                          ; preds = %entry
  br label %return

if.end:                                           ; preds = %entry
  br label %return

return:                                           ; preds = %if.end, %if.then
  %retval.0 = phi i32 [ 1, %if.then ], [ 0, %if.end ]
  ret i32 %retval.0
}

attributes #0 = { noinline nounwind uwtable}
