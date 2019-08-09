#!/bin/bash


rm -rf a.out

ifort -coarray=distributed -coarray-config-file=caf_config.txt get_hostnames.f90