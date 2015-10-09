#!/bin/sh

export RISCV_HOME=`pwd`
export RISCV_BUILD=$RISCV_HOME/build
export RISCV_TOOLS=$RISCV_BUILD/tools
export RISCV_VENDOR=$RISCV_HOME/vendor
export RISCV_VENDOR_CONNECTAL=$RISCV_VENDOR/connectal
export PATH=$PATH:$RISCV_TOOLS/bin:$RISCV_VENDOR/android-ndk-r10e:$RISCV_VENDOR/android-sdk-linux/tools
