
#include <jni.h>
#include <stdlib.h>
#include <iostream>
#include <string>

extern "C" {
extern char *classObjectBuffer;

/*
 *  ClassObject Packet format:
 *  <int>SIZE</int><byte>OBJECT STREAM</byte>
 */

JNIEXPORT void JNICALL Java_jasmin_Main_llc_1dump_1classObj(JNIEnv *env,
                                                            jobject obj,
                                                            jbyteArray bytes) {
  jint len = env->GetArrayLength(reinterpret_cast<jarray *>(&bytes)[0]);
  classObjectBuffer = (char *)malloc(sizeof(int) + len + 1);

  int *payLoadSize = (int *)classObjectBuffer;
  char *objectPayLoad = classObjectBuffer + sizeof(int);

  std::cout << "JNI Call : Java_jasmin_Main_llc_1dump_1classObj .."
            << std::endl;
  std::cout << "JNI Call : Created buffer of size " << len << " .."
            << std::endl;

  env->GetByteArrayRegion(bytes, 0, len, (jbyte *)objectPayLoad);
  objectPayLoad[len] = 0; /* NULL-terminate payload */
  payLoadSize[0] = len;

  std::cout << "Copied ClassObject Packet to classObjectBuffer\n"
            << "Size = " << payLoadSize[0] << "\n"
            << "Contents = " << classObjectBuffer << std::endl;
  fwrite(((void *)classObjectBuffer), len, 1, stdout);
}
}

