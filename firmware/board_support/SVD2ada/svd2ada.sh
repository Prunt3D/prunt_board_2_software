#!/usr/bin/env bash

# This shell script creates embedded runtimes and spec files for ARM
# Cortex-M microcontrollers taking into account the directory structure
# of bb-runtimes and Ada Drivers Library from AdaCore.
# The directory structure will be created inside the current folder,
# where resides the "svd2ada" executable file with its "schema" folder.
# Actually this current folder is "SVD2ada".

# Directory where svd2ada is located
SVD2ADA_DIR=`pwd`

# Name of the target ARM svd file.
# This file is located at $SVD2ADA_DIR/CMSIS-SVD/ST/
TARGET_SVD=CMSIS-SVD/ST/STM32G474xx.svd

# Name of the target folder
TARGET=stm32g474

# Run-Time path
#RT_DIR=../Ada_Drivers_Library/repo/bb-runtimes/arm/stm32/$TARGET/svd
RT_DIR=i-svd

# Ada Drivers Library target path
#ADL_DIR=../Ada_Drivers_Library/repo/arch/ARM/STM32/svd/$TARGET
ADL_DIR=svd

env1="rm -rf $RT_DIR;
      mkdir -p $RT_DIR/tmp;
      $SVD2ADA_DIR/svd2ada $SVD2ADA_DIR/$TARGET_SVD -o $RT_DIR/tmp -p Interfaces.STM32;
      cp $RT_DIR/tmp/a-intnam.ads $RT_DIR;
      cp $RT_DIR/tmp/handler.S $RT_DIR;
      cp $RT_DIR/tmp/i-stm32.ads $RT_DIR;
      cp $RT_DIR/tmp/i-stm32-flash.ads $RT_DIR;
      cp $RT_DIR/tmp/i-stm32-gpio.ads $RT_DIR;
      cp $RT_DIR/tmp/i-stm32-pwr.ads $RT_DIR;
      cp $RT_DIR//tmp/i-stm32-rcc.ads $RT_DIR;
      cp $RT_DIR/tmp/i-stm32-syscfg.ads $RT_DIR;
      cp $RT_DIR/tmp/i-stm32-usart.ads $RT_DIR;
      rm -rf $RT_DIR/tmp"

env2="rm -rf $ADL_DIR;
      mkdir -p $ADL_DIR;
      $SVD2ADA_DIR/svd2ada $SVD2ADA_DIR/$TARGET_SVD -o $ADL_DIR -p STM32_SVD --boolean --base-types-package HAL --gen-uint-always"

msg1="Generated embedded run-time files to $SVD2ADA_DIR/$RT_DIR"
msg2="Generated target spec files to $SVD2ADA_DIR/$ADL_DIR"

print_only=false

while [ "$#" -gt 0 ]; do
  optname=$1

  case $optname in
    --print-only) print_only=true;;
    --help)
      echo "Usage: svd2ada.sh [options]"
      echo ""
      echo "  --print-only  output on stdout a script than can be evaluated"
      echo "                to set the svd2ada"
      echo "  --runtime     generate embedded runtime files to $RT_DIR"
      echo "  --spec        generate target spec files to $ADL_DIR"
      echo "  --all         generate both embedded runtime and spec files"
      echo "  --remove-all  remove both embedded runtime and spec files"
      exit 0
      ;;
    --runtime)
      env=$env1
      msg=$msg1
      ;;
    --spec)
      env=$env2
      msg=$msg2
      ;;
    --all)
      env="$env1; echo ""; echo $msg1; echo ""; $env2; echo ""; echo $msg2"
      msg=""
      ;;
    --remove-all)
      env="rm -rf $RT_DIR $ADL_DIR"
      msg="Removed both runtime and spec files"
      ;;
    *) echo "unsupported option $optname"
      exit 1
      ;;
  esac
  shift 1
done

if [[ print_only = "true" ]]; then
  echo ""
  echo "Generate embedded runtime files to $RT_DIR:"
  echo ""
  echo "$env1"
  echo ""
  echo "Generate target spec files to $SPEC_DIR:"
  echo ""
  echo "$env2"
  echo ""
  echo "Generate both embedded runtime and spec files."
  echo ""
  echo "Remove both embedded runtime and spec files."
  echo ""
else
  eval "$env"
  echo "$msg"
fi
