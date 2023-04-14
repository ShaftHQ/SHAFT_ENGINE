#!/bin/bash

if ! command -v node &> /dev/null; then
    echo "Node.js is not installed, please download Node.js"
    exit 1
fi

if ! npm list -g --depth 0 lighthouse &> /dev/null; then
    npm install -g lighthouse
fi

if ! npm list -g --depth 0 puppeteer &> /dev/null; then
    npm install -g puppeteer
fi

if ! npm list -g --depth 0 optimist &> /dev/null; then
    npm install -g optimist
fi

if ! npm list -g --depth 0 fs &> /dev/null; then
    npm install -g fs
fi

if ! npm list -g --depth 0 open &> /dev/null; then
    npm install -g open
fi
exit 0
