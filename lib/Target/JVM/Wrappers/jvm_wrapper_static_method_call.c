
#include "stdlib.h"
#include "unistd.h"
#include <jni.h>

void jvm_wrapper_static_method_call(char *byteArray, char *className, char *methodName) {
  jint res;
  jclass cls;
  JNIEnv *env;
  JavaVM *jvm;
  jmethodID mid;
  int result = 0;
  JavaVMInitArgs vm_args;
  JavaVMOption options[4];

  options[0].optionString = "-Djava.class.path=.";
  options[1].optionString = "-Dsun.java.launcher.diag=true";
  options[2].optionString = "-Dsun.java.launcher=SUN_STANDARD";
#if 1
				options[3].optionString = 
								"-XX:+PauseAtStartup -Xlog:all=trace";
#endif
  vm_args.version = 0x00010002;
  vm_args.options = options;
  vm_args.nOptions = 4;
  vm_args.ignoreUnrecognized = JNI_TRUE;

  fprintf(stdout, "ClassName = %s, MethodName = %s...\n", className,
          methodName);
  /* Create the Java VM */
  res = JNI_CreateJavaVM(&jvm, (void **)&env, &vm_args);
  if (res < 0) {
    fprintf(stderr, "Can't create Java VM\n");
    exit(1);
  }
#if 0
    sleep(20);
#endif
  cls = (*env)->FindClass(env, className);
  if (cls == NULL) {
    goto destroy;
  }
  fprintf(stdout, "Found Class...\n");
  mid = (*env)->GetStaticMethodID(env, cls, methodName, "([B)V");
  if (mid == NULL) {
    goto destroy;
  }
  jbyteArray jBArr = (*env)->NewByteArray(env, 256);
  if (jBArr != NULL) {
    (*env)->SetByteArrayRegion(env, jBArr, 0, 256, (jbyte *)byteArray);
  }
  (*env)->CallStaticVoidMethod(env, cls, mid, jBArr);
  (*env)->GetByteArrayRegion(env, jBArr, 0, 256, (jbyte *)byteArray);
  (*env)->DeleteLocalRef(env, jBArr);
destroy:
  if ((*env)->ExceptionOccurred(env)) {
    (*env)->ExceptionDescribe(env);
  }
  (*jvm)->DestroyJavaVM(jvm);
  return;
}
