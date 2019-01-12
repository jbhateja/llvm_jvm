; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py

; RUN: llc < %s -mtriple=jvm32-unknown-unknown -jvm-candidate-checker | FileCheck %s --check-prefix=JVM -match-full-lines

%struct.S1 = type <{ i8, i64 }>

; Function Attrs: noinline nounwind uwtable
define dso_local signext i8 @getCharValAt(%struct.S1* byval align 8 %obj) #0 {
; JVM:       aload 0
; JVM-NEXT:    getfield AoCClass1/f1 J
; JVM-NEXT:    lstore 1
; JVM-NEXT:    aload 0
; JVM-NEXT:    lload 1
; JVM-NEXT:    ldc2_w -16711681
; JVM-NEXT:    land
; JVM-NEXT:    lstore 3
; JVM-NEXT:    lload 3
; JVM-NEXT:    ldc2_w 4259840
; JVM-NEXT:    lor
; JVM-NEXT:    lstore 5
; JVM-NEXT:    lload 5
; JVM-NEXT:    putfield AoCClass1/f1 J
; JVM-NEXT:    aload 0
; JVM-NEXT:    getfield AoCClass1/f1 J
; JVM-NEXT:    lstore 7
; JVM-NEXT:    lload 7
; JVM-NEXT:    ldc2_w 16711680
; JVM-NEXT:    land
; JVM-NEXT:    lstore 9
; JVM-NEXT:    lload 9
; JVM-NEXT:    ldc 16
; JVM-NEXT:    lushr
; JVM-NEXT:    lstore 11
; JVM-NEXT:    lload 11
; JVM-NEXT:    l2i
; JVM-NEXT:    istore 13
; JVM-NEXT:    iload 13
; JVM-NEXT:    i2b
; JVM-NEXT:    istore 14
; JVM-NEXT:    iload 14
; JVM-NEXT:    istore 15
; JVM-NEXT:    iload 15
; JVM-NEXT:    ireturn
entry:
  %0 = bitcast %struct.S1* %obj to i8*
  %arrayidx = getelementptr inbounds i8, i8* %0, i64 3
  store i8 65, i8* %arrayidx, align 1
  %arrayidx1 = getelementptr inbounds i8, i8* %0, i64 3
  %1 = load i8, i8* %arrayidx1, align 1
  ret i8 %1
}

attributes #0 = { noinline nounwind uwtable }
