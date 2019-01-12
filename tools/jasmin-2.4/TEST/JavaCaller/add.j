.source noSource
"add"
.class public AddInstructions
.super java/lang/Object

.method public AddInstructions()V
  .limit stack 1
  .limit locals 1
   aload_0
  invokespecial java/lang/Object."<init>"()V
  return
.end method

.method public static add(II)I
  .limit stack 2
  .limit locals 3
  iload_0
  iload_1
  iadd
  istore_2
  getstatic java/lang/System/out Ljava/io/PrintStream;
  iload_2
  invokevirtual java/io/PrintStream/println(I)V
  iload_2
  ireturn
.end method
