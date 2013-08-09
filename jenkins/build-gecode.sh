#!/bin/bash -xe

# Build gecode deb.

# This is not working. It is building into vagrant. Test directly?

PROJ_NAME=gecode
GECODE_VERSION="3.7.3"
GECODE_SRC_URL="http://www.gecode.org/download/gecode-${GECODE_VERSION}.tar.gz"
GECODE_SRC="gecode-${GECODE_VERSION}.tar.gz"
FAKE_ROOT=${WORKSPACE}/fake_root/

export PATH=$PATH:/usr/local/bin
jenkins/builder_info.rb
source machine_info
ARTIFACT_BASE=opscode-ci/artifacts/$os/$machine/$PROJ_NAME
GECODE_DEB="gecode-${GECODE_VERSION}-${os}-${machine}.deb"

cd ${WORKSPACE}
if [ ! -f ${GECODE_SRC} ]; then
  wget ${GECODE_SRC_URL} || exit 1
  tar zxvf ${GECODE_SRC}
fi

cd gecode-${GECODE_VERSION}
make distclean || echo "Build source already clean"
./configure \
   --prefix=/opt/gecode \
   --disable-doc-dot \
   --disable-doc-search \
   --disable-doc-tagfile \
   --disable-doc-chm \
   --disable-doc-docset \
   --disable-qt \
   --disable-examples
mkdir -p ${FAKE_ROOT}
make install DESTDIR=${FAKE_ROOT} || exit 1

cd ${WORKSPACE}/jenkins

if [ -f ${GECODE_DEB} ]; then
  rm ${GECODE_DEB}
fi

fpm -s dir -t deb -n ${PROJ_NAME} -v ${GECODE_VERSION} -C ${FAKE_ROOT} \
  -p ${GECODE_DEB} \
  opt/gecode

s3cmd put --progress $GECODE_DEB s3://$ARTIFACT_BASE/$GECODE_DEB

