; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py

; RUN: llc < %s -mtriple=jvm32-unknown-unknown | FileCheck %s --check-prefix=JVM

; Function Attrs: noinline nounwind uwtable
define i32 @Mux(i32 %cond) #0 {
; JVM:       iload 0
; JVM-NEXT:    iconst_1
; JVM-NEXT:    if_icmplt LBB0_2
; JVM-NEXT:    goto LBB0_1
; JVM-NEXT:  LBB0_1:
; JVM-NEXT:    ldc 10
; JVM-NEXT:    istore 1
; JVM-NEXT:    goto LBB0_3
; JVM-NEXT:  LBB0_2:
; JVM-NEXT:    ldc 20
; JVM-NEXT:    istore 1
; JVM-NEXT:  LBB0_3:
; JVM-NEXT:    iload 1
; JVM-NEXT:    istore 2
; JVM-NEXT:    iload 2
; JVM-NEXT:    ireturn
entry:
  %cmp = icmp sgt i32 %cond, 0
  br i1 %cmp, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  br label %if.end

if.else:                                          ; preds = %entry
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  %mux.0 = phi i32 [ 10, %if.then ], [ 20, %if.else ]
  ret i32 %mux.0
}
attributes #0 = { noinline nounwind uwtable}
