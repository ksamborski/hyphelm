#!/bin/bash

for i in {0..28} ; do node --harmony hypelm-test_data.js $i >> data.json ; done
