; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple=jvm32-unknown-unknown -jvm-candidate-checker | FileCheck %s --check-prefix=JVM -match-full-lines

; Function Attrs: noinline nounwind
define dso_local i32 @getValAt() #0 {
; JVM:       ldc 10
; JVM-NEXT:    newarray int
; JVM-NEXT:    astore 1
; JVM-NEXT:    aload 1
; JVM-NEXT:    bipush 0
; JVM-NEXT:    ldc 305419776
; JVM-NEXT:    iastore
; JVM-NEXT:    aload 1
; JVM-NEXT:    bipush 0
; JVM-NEXT:    iaload
; JVM-NEXT:    istore 2
; JVM-NEXT:    aload 1
; JVM-NEXT:    bipush 0
; JVM-NEXT:    iload 2
; JVM-NEXT:    ldc -256
; JVM-NEXT:    iand
; JVM-NEXT:    istore 3
; JVM-NEXT:    iload 3
; JVM-NEXT:    ldc 65
; JVM-NEXT:    ior
; JVM-NEXT:    istore 4
; JVM-NEXT:    iload 4
; JVM-NEXT:    iastore
; JVM-NEXT:    aload 1
; JVM-NEXT:    bipush 0
; JVM-NEXT:    iaload
; JVM-NEXT:    istore 5
; JVM-NEXT:    iload 5
; JVM-NEXT:    ireturn
entry:
  %arr = alloca [10 x i32], align 4
  %arrayidx = getelementptr inbounds [10 x i32], [10 x i32]* %arr, i32 0, i32 0
  store i32 305419776, i32* %arrayidx, align 4
  %arraydecay = getelementptr inbounds [10 x i32], [10 x i32]* %arr, i32 0, i32 0
  %0 = bitcast i32* %arraydecay to i8*
  %arrayidx1 = getelementptr inbounds i8, i8* %0, i32 0
  store i8 65, i8* %arrayidx1, align 1
  %arrayidx2 = getelementptr inbounds [10 x i32], [10 x i32]* %arr, i32 0, i32 0
  %1 = load i32, i32* %arrayidx2, align 4
  ret i32 %1
}
attributes #0 = { noinline nounwind }
