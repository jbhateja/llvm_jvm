//===-- JVMAssembler.cpp - Implementation for JVMAssembler ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//                         Jasmin based JVMAssembler
//===----------------------------------------------------------------------===//

#include "JVMAssembler.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/DynamicLibrary.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/Process.h"
#include <jni.h>

#define DEBUG_TYPE "jvm-assembler"

extern "C" {
char *classObjectBuffer = nullptr;
}

using namespace llvm;
using namespace sys;

namespace llvm {

class JVMAssemblerImpl {
public:
  using CreateJavaVM = jint (*)(JavaVM **, void **, void *);

  class JVMExceptionChecker {
  public:
    JVMExceptionChecker(JNIEnv *jenv) : env(jenv) {}

    ~JVMExceptionChecker() {
      if (env->ExceptionOccurred()) {
        env->ExceptionDescribe();
      }
    }

  private:
    JNIEnv *env;
  };

  JVMAssemblerImpl();
  JVMAssemblerImpl(const JVMAssemblerImpl &) = delete;
  JVMAssemblerImpl &operator=(const JVMAssemblerImpl &) = delete;
  ~JVMAssemblerImpl();

  char *assemble(std::string &asmFile);
  void freeClassObjectBuffer();

  static JVMAssemblerImpl *inst;
  static JVMAssemblerImpl *getInst() {
    if (!inst) {
      inst = new JVMAssemblerImpl();
    }
    return inst;
  }

private:
  JNIEnv *env;
  JavaVM *jvm;
};
} // namespace llvm

void JVMAssemblerImpl::freeClassObjectBuffer() {
  if (classObjectBuffer)
    free(classObjectBuffer);
  classObjectBuffer = nullptr;
}

JVMAssemblerImpl *JVMAssemblerImpl::inst = nullptr;

JVMAssemblerImpl::~JVMAssemblerImpl() {
  jvm->DestroyJavaVM();
  freeClassObjectBuffer();
}

JVMAssemblerImpl::JVMAssemblerImpl() {
  JavaVMInitArgs vm_args;
  JavaVMOption options[5];
  std::string libPath = "-Djava.library.path=";
  std::string classPath = "-Djava.class.path=";

  if (!Process::GetEnv("JASMIN_HOME").hasValue())
    report_fatal_error("environment variable JASMIN_HOME  not defined");

  classPath = classPath + Process::GetEnv("JASMIN_HOME").getValue();
  classPath = classPath + "/jasmin.jar";
  libPath = libPath + Process::GetEnv("JASMIN_HOME").getValue();

  classObjectBuffer = nullptr;

  llvm::outs() << classPath;
  llvm::outs() << libPath;

  options[0].optionString = const_cast<char *>(classPath.c_str());
  options[1].optionString = "-Dsun.java.command=jasmin.jar";
  options[2].optionString = "-Dsun.java.launcher.diag=true";
  options[3].optionString = "-Dsun.java.launcher=SUN_STANDARD";
  options[4].optionString = const_cast<char *>(libPath.c_str());

  vm_args.version = 0x00010002;
  vm_args.options = options;
  vm_args.nOptions = 5;
  vm_args.ignoreUnrecognized = JNI_TRUE;

  DynamicLibrary lib = DynamicLibrary::getPermanentLibrary("libjvm.so");
  if (!lib.isValid())
    report_fatal_error("Cannot find libjvm.so");

  CreateJavaVM createVM =
      (CreateJavaVM)lib.getAddressOfSymbol("JNI_CreateJavaVM");
  if (!createVM)
    report_fatal_error("Cannot find symbol JNI_CreateJavaVM");

  /* Create the Java VM */
  if (JNI_OK != createVM(&jvm, (void **)&env, &vm_args))
    report_fatal_error("Cannot load JVM");
}

char *JVMAssemblerImpl::assemble(std::string &asmFile) {
  JVMAssemblerImpl::JVMExceptionChecker checker(env);
  jclass cls = env->FindClass("jasmin/Main");
  if (NULL == cls)
    return nullptr;

  DEBUG(llvm::dbgs() << "\nInvoking jasmin-assemble.\n");
  jmethodID cnstr = env->GetMethodID(cls, "<init>", "()V");
  if (NULL == cnstr)
    return nullptr;

  jobject jobj = env->NewObject(cls, cnstr);
  if (NULL == jobj)
    return nullptr;

  jmethodID mid =
      env->GetMethodID(cls, "assemble_string", "(Ljava/lang/String;)V");
  if (NULL == mid)
    return nullptr;

  jstring jstr = env->NewStringUTF(asmFile.c_str());
  if (NULL == jstr)
    return nullptr;

  DEBUG(llvm::dbgs() << "\nJASM size = %d.\n"
                     << strlen(asmFile.c_str()) << "\n");
  env->CallVoidMethod(jobj, mid, jstr);

  if (!classObjectBuffer)
    report_fatal_error("Error while assembling, empty classObjectBuffer");
  return classObjectBuffer;
}

JVMAssembler::JVMAssembler() {
  Impl = JVMAssemblerImpl::getInst();
  DEBUG(llvm::dbgs() << "JavaVM created.\n");
}

JVMAssembler::~JVMAssembler() { delete Impl; }

Optional<std::unique_ptr<JVMAssembler::JVMClassPacket>>
JVMAssembler::AssembleClass(std::string asmFile) {
  std::unique_ptr<JVMAssembler::JVMClassPacket> Packet;

  char *classObject = Impl->assemble(asmFile);
  if (!classObject)
    return None;

  Packet = llvm::make_unique<JVMAssembler::JVMClassPacket>(classObject);

  Impl->freeClassObjectBuffer();
  return std::move(Packet);
}

JVMAssembler::JVMClassPacket::JVMClassPacket(char *packet) {
  size = ((int *)packet)[0];
  payload = new char[size];
  memcpy(payload, packet + sizeof(int), size);
}

JVMAssembler::JVMClassPacket &&JVMAssembler::JVMClassPacket::
operator=(const JVMClassPacket &&obj) {
  JVMClassPacket &newPkt = *this;
  newPkt.size = obj.size;
  newPkt.payload = obj.payload;
  return std::move(newPkt);
}

JVMAssembler::JVMClassPacket::JVMClassPacket(
    const JVMAssembler::JVMClassPacket &&obj) {
  JVMClassPacket &newPkt = *this;
  newPkt = std::move(obj);
}

void JVMAssembler::JVMClassPacket::dump() {
  llvm::outs() << "\nJVMClassPacket:\nSize = " << size << "\n";
  llvm::outs() << "Payload = "
               << "\n";
  fwrite(payload, size, 1, stdout);
  llvm::outs() << "\n";
}
