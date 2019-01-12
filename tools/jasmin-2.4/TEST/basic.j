; ASM file start. -> JVMAsmPrinter->
.source noSource
.class public static BasicInstructions
.super java/lang/Object


;Function Start
.method public static main([Ljava/lang/String;)V
  .limit stack 5
  .limit locals 3

; InstPrinter
  sipush 5 ;push integer 5 onto stack
  istore 0 ;pop integer 5 and store in index 0
  ldc "Hello World" ;push string Hello World onto stack
  astore 1 ;store string in index 1
  iload 0 ;load 5
  dup ;duplicate stack entry 5
  sipush 2 ;push integer 2 onto stack
  isub ;pop 5 and 2 and store result 3 onto stack
  iadd ;pop 5 and 3 and store result 8 onto stack
  istore 0 ;store result 8 in index 0
  return

; Function End
.end method


; ASM file end.
