#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DIST_DIR=${DIR}/.dist

PACKAGE_NAME=Asn1Acn-PusC-Library

rm -rf ${DIST_DIR}
mkdir -p ${DIST_DIR}

rsync -a --prune-empty-dirs \
      --include '*/' \
      --include '*.asn1' \
      --include '*.acn' \
      --include '*.json' \
      --include '*.c' \
      --include '*.h' \
      --exclude '*' \
      ${DIR}/* ${DIST_DIR}

cp ${DIR}/LICENSE ${DIST_DIR}
cp ${DIR}/README.md ${DIST_DIR}

pushd ${DIST_DIR}
tar czf ${DIR}/${PACKAGE_NAME}-${TRAVIS_TAG}.tar.gz *
7z a ${DIR}/${PACKAGE_NAME}-${TRAVIS_TAG}.zip *
7z a ${DIR}/${PACKAGE_NAME}-${TRAVIS_TAG}.7z *
popd
