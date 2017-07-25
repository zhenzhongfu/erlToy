#!/bin/bash

## 生成protobuf的beam和hrl

SCRIPTDIR=`cd $(dirname $0);pwd`
ROOT=${SCRIPTDIR}/..
PROTODIR=${ROOT}/proto
INCLUDEDIR=${ROOT}/include/proto
SRCDIR=${ROOT}/src/proto

if [ ! -d ${INCLUDEDIR} ];then
    mkdir ${INCLUDEDIR}
fi

gen() {
    OPT=$1
    ${SCRIPTDIR}/gen_proto.escript ${GENTYPE} ${PROTODIR} ${INCLUDEDIR} ${SRCDIR} ${PROTO_EBINDIR} ${OUT_EBINDIR}
}

usage() {
    echo "usage: ./gen GENTYPE PROTOEBINDIR OUTEBINDIR\
                GENTYPE = beam | code"
}

GENTYPE=$1
PROTO_EBINDIR=$2
OUT_EBINDIR=$3
case ${GENTYPE} in
'beam') gen beam $2 $3;;
'code') gen code $2 $3;;
*) usage; exit 1;;
esac
