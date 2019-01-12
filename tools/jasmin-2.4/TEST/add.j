.source noSource
.class public static AddInstructions
.super java/lang/Object
.method public static main([Ljava/lang/String;)V
  .limit stack 2
  .limit locals 3
  iconst_1 
  iconst_2
  iadd
  istore_0
  iconst_3
  iconst_0
  iadd 
  istore_1
  iload_0
  iload_1
  iadd
  istore_2
  getstatic java/lang/System/out Ljava/io/PrintStream;
  iload_2
  invokevirtual java/io/PrintStream/println(I)V
  return
.end method
