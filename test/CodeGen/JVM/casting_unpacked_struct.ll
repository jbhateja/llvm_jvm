; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py

; RUN: llc < %s -mtriple=jvm32-unknown-unknown -jvm-candidate-checker | FileCheck %s --check-prefix=JVM -match-full-lines

%struct.S1 = type { i8, i64 }

; Function Attrs: noinline nounwind uwtable
define dso_local signext i8 @getCharValAt(i8 %obj.coerce0, i64 %obj.coerce1) #0 {
; JVM:       new AoCClass1
; JVM-NEXT:    dup
; JVM-NEXT:    invokenonvirtual AoCClass1/<init>()V
; JVM-NEXT:    astore 4
; JVM-NEXT:    aload 4
; JVM-NEXT:    iload 0
; JVM-NEXT:    putfield AoCClass1/f0 B
; JVM-NEXT:    aload 4
; JVM-NEXT:    lload 1
; JVM-NEXT:    putfield AoCClass1/f1 J
; JVM-NEXT:    aload 4
; JVM-NEXT:    ldc 65
; JVM-NEXT:    putfield AoCClass1/f0 B
; JVM-NEXT:    aload 4
; JVM-NEXT:    getfield AoCClass1/f0 B
; JVM-NEXT:    istore 5
; JVM-NEXT:    iload 5
; JVM-NEXT:    istore 6
; JVM-NEXT:    iload 6
; JVM-NEXT:    ireturn
entry:
  %obj = alloca %struct.S1, align 8
  %0 = bitcast %struct.S1* %obj to { i8, i64 }*
  %1 = getelementptr inbounds { i8, i64 }, { i8, i64 }* %0, i32 0, i32 0
  store i8 %obj.coerce0, i8* %1, align 8
  %2 = getelementptr inbounds { i8, i64 }, { i8, i64 }* %0, i32 0, i32 1
  store i64 %obj.coerce1, i64* %2, align 8
  %3 = bitcast %struct.S1* %obj to i8*
  %arrayidx = getelementptr inbounds i8, i8* %3, i64 3
  store i8 65, i8* %arrayidx, align 1
  %arrayidx1 = getelementptr inbounds i8, i8* %3, i64 3
  %4 = load i8, i8* %arrayidx1, align 1
  ret i8 %4
}

attributes #0 = { noinline nounwind uwtable }
