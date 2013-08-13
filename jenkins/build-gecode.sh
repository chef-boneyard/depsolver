#!/bin/bash -xe
#  -x xtrace, print each command and expand
#  -e errexit, abort script at first error

# Build gecode deb.

PROJ_NAME=gecode
GECODE_VERSION="3.7.3"
GECODE_SRC_URL="http://www.gecode.org/download/gecode-${GECODE_VERSION}.tar.gz"
GECODE_SRC="gecode-${GECODE_VERSION}.tar.gz"

# Use a different name for the package so we can keep the system libgecode
# Remove this when opscode-chef and Ruby depselector are no longer needed
GECODE_DEB_NAME="gecode-opscode"

# Define FAKE_ROOT so that fpm can package this up as if it were located in /opt/gecode rather
# than a temporary (but writable) workspace.
FAKE_ROOT=${WORKSPACE}/fake_root/

export PATH=$PATH:/usr/local/bin

# Get $machine and $os, usually
#   machine - x86_64
#   os      - ubuntu-10.04
jenkins/builder_info.rb
source machine_info

ARTIFACT_BASE=opscode-ci/artifacts/$os/$machine/$PROJ_NAME
GECODE_DEB="gecode-${GECODE_VERSION}-${os}-${machine}.deb"

cd ${WORKSPACE}
if [ ! -f ${GECODE_SRC} ]; then
  wget ${GECODE_SRC_URL}
  tar zxf ${GECODE_SRC}
fi

cd gecode-${GECODE_VERSION}
make distclean || echo "Build source already clean"

# Configure flags are pulled from opscode-omnibus
# Make sure this is in sync
./configure \
   --prefix=/opt/gecode \
   --disable-doc-dot \
   --disable-doc-search \
   --disable-doc-tagfile \
   --disable-doc-chm \
   --disable-doc-docset \
   --disable-qt \
   --disable-examples

# Use DESTDIR to override the make installation. It will still
# install into the prefix ${FAKE_ROOT}/opt/gecode
mkdir -p ${FAKE_ROOT}
make install DESTDIR=${FAKE_ROOT}

cd ${WORKSPACE}/jenkins
rm -f ${GECODE_DEB}

# Make deb package. Point FPM to the fake root and only package up opt/gecode
# The resulting deb will install to /opt/gecode
fpm -s dir -t deb -n ${GECODE_DEB_NAME} -v ${GECODE_VERSION} -C ${FAKE_ROOT} \
  -p ${GECODE_DEB} \
  opt/gecode

s3cmd put $GECODE_DEB s3://$ARTIFACT_BASE/$GECODE_DEB

