; ASM file start. -> JVMAsmPrinter->
.source noSource
.class public static BasicComposite
.super java/lang/Object

;Function Start
.method public static main([Ljava/lang/String;)V
  .limit stack 5
  .limit locals 3

; InstPrinter
  iconst_5
  newarray int
  aastore 0

  aload 0
  ldc 2
  iastore 10
  
  aload 0
  ldc 2
  iaload 
  istore 1

  getstatic java/lang/System/out Ljava/io/PrintStream;
  iload 1
  invokevirtual java/io/PrintStream/println(I)V

; Function End
.end method


; ASM file end.
