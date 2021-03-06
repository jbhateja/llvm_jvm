; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py

; RUN: llc < %s -mtriple=jvm32-unknown-unknown -jvm-candidate-checker | FileCheck %s --check-prefix=JVM -match-full-lines

define i1 @func(i32 %a , i32 %b) {
; JVM:       iload 0
; JVM-NEXT:    iload 1
; JVM-NEXT:    if_icmpeq LBB0_2
; JVM-NEXT:    goto LBB0_1
; JVM-NEXT:  LBB0_1:
; JVM-NEXT:    ldc 1
; JVM-NEXT:    istore 2
; JVM-NEXT:    goto LBB0_3
; JVM-NEXT:  LBB0_2:
; JVM-NEXT:    ldc 0
; JVM-NEXT:    istore 2
; JVM-NEXT:  LBB0_3:
; JVM-NEXT:    iload 2
; JVM-NEXT:    istore 3
; JVM-NEXT:    iload 3
; JVM-NEXT:    i2b
; JVM-NEXT:    istore 4
; JVM-NEXT:    iload 4
; JVM-NEXT:    ireturn
  %res = icmp ne i32 %a , %b
  ret i1 %res
}
