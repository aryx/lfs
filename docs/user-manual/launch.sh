#!/bin/sh

latex main.tex && makeindex -s index.ist main.idx && latex main.tex && dvips main.dvi -o main.ps && gv main.ps