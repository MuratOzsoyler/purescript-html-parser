/* global exports */
"use strict";

// module Test.Utils

export function exit(exitCode) {
  return function () {
    process.exit(exitCode);
  };
}

var globalExitStatus = 0;

export function setExitStatus(exitCode) {
  return function () {
    globalExitStatus = exitCode;
  };
}

export function getExitStatus() {
  return globalExitStatus;
}
