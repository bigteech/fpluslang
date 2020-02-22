/******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId]) {
/******/ 			return installedModules[moduleId].exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			i: moduleId,
/******/ 			l: false,
/******/ 			exports: {}
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.l = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// define getter function for harmony exports
/******/ 	__webpack_require__.d = function(exports, name, getter) {
/******/ 		if(!__webpack_require__.o(exports, name)) {
/******/ 			Object.defineProperty(exports, name, { enumerable: true, get: getter });
/******/ 		}
/******/ 	};
/******/
/******/ 	// define __esModule on exports
/******/ 	__webpack_require__.r = function(exports) {
/******/ 		if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
/******/ 			Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
/******/ 		}
/******/ 		Object.defineProperty(exports, '__esModule', { value: true });
/******/ 	};
/******/
/******/ 	// create a fake namespace object
/******/ 	// mode & 1: value is a module id, require it
/******/ 	// mode & 2: merge all properties of value into the ns
/******/ 	// mode & 4: return value when already ns object
/******/ 	// mode & 8|1: behave like require
/******/ 	__webpack_require__.t = function(value, mode) {
/******/ 		if(mode & 1) value = __webpack_require__(value);
/******/ 		if(mode & 8) return value;
/******/ 		if((mode & 4) && typeof value === 'object' && value && value.__esModule) return value;
/******/ 		var ns = Object.create(null);
/******/ 		__webpack_require__.r(ns);
/******/ 		Object.defineProperty(ns, 'default', { enumerable: true, value: value });
/******/ 		if(mode & 2 && typeof value != 'string') for(var key in value) __webpack_require__.d(ns, key, function(key) { return value[key]; }.bind(null, key));
/******/ 		return ns;
/******/ 	};
/******/
/******/ 	// getDefaultExport function for compatibility with non-harmony modules
/******/ 	__webpack_require__.n = function(module) {
/******/ 		var getter = module && module.__esModule ?
/******/ 			function getDefault() { return module['default']; } :
/******/ 			function getModuleExports() { return module; };
/******/ 		__webpack_require__.d(getter, 'a', getter);
/******/ 		return getter;
/******/ 	};
/******/
/******/ 	// Object.prototype.hasOwnProperty.call
/******/ 	__webpack_require__.o = function(object, property) { return Object.prototype.hasOwnProperty.call(object, property); };
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(__webpack_require__.s = "./src/browsershell.fsproj");
/******/ })
/************************************************************************/
/******/ ({

/***/ "./src/App.fs":
/*!********************!*\
  !*** ./src/App.fs ***!
  \********************/
/*! no static exports found */
/***/ (function(module, exports) {

eval("throw new Error(\"Module build failed (from ./node_modules/fable-loader/index.js):\\nError: BABEL ERROR: Failed to parse macro: $0.[$1] -> $0.[$1]\\nMACRO ARGUMENTS: $0,$1\\nUnexpected token (2:3)\\n  1 | /* @babel/template */;\\n> 2 | $0.[$1]\\n    |   ^\\n    at PluginPass.StringLiteral (/Users/wangweipeng/fc/browsershell/node_modules/fable-babel-plugins/index.js:128:23)\\n    at newFn (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/visitors.js:193:21)\\n    at NodePath._call (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/path/context.js:53:20)\\n    at NodePath.call (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/path/context.js:40:17)\\n    at NodePath.visit (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/path/context.js:88:12)\\n    at TraversalContext.visitQueue (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/context.js:118:16)\\n    at TraversalContext.visitSingle (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/context.js:90:19)\\n    at TraversalContext.visit (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/context.js:146:19)\\n    at Function.traverse.node (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/index.js:94:17)\\n    at NodePath.visit (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/path/context.js:95:18)\\n    at TraversalContext.visitQueue (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/context.js:118:16)\\n    at TraversalContext.visitMultiple (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/context.js:85:17)\\n    at TraversalContext.visit (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/context.js:144:19)\\n    at Function.traverse.node (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/index.js:94:17)\\n    at NodePath.visit (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/path/context.js:95:18)\\n    at TraversalContext.visitQueue (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/context.js:118:16)\\n    at TraversalContext.visitMultiple (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/context.js:85:17)\\n    at TraversalContext.visit (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/context.js:144:19)\\n    at Function.traverse.node (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/index.js:94:17)\\n    at NodePath.visit (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/path/context.js:95:18)\\n    at TraversalContext.visitQueue (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/context.js:118:16)\\n    at TraversalContext.visitSingle (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/context.js:90:19)\\n    at TraversalContext.visit (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/context.js:146:19)\\n    at Function.traverse.node (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/index.js:94:17)\\n    at NodePath.visit (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/path/context.js:95:18)\\n    at TraversalContext.visitQueue (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/context.js:118:16)\\n    at TraversalContext.visitMultiple (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/context.js:85:17)\\n    at TraversalContext.visit (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/context.js:144:19)\\n    at Function.traverse.node (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/index.js:94:17)\\n    at NodePath.visit (/Users/wangweipeng/fc/browsershell/node_modules/@babel/traverse/lib/path/context.js:95:18)\");\n\n//# sourceURL=webpack:///./src/App.fs?");

/***/ }),

/***/ "./src/browsershell.fsproj":
/*!*********************************!*\
  !*** ./src/browsershell.fsproj ***!
  \*********************************/
/*! no static exports found */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
eval("__webpack_require__.r(__webpack_exports__);\n/* harmony import */ var _App_fs__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./App.fs */ \"./src/App.fs\");\n/* harmony import */ var _App_fs__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(_App_fs__WEBPACK_IMPORTED_MODULE_0__);\n/* harmony reexport (unknown) */ for(var __WEBPACK_IMPORT_KEY__ in _App_fs__WEBPACK_IMPORTED_MODULE_0__) if(__WEBPACK_IMPORT_KEY__ !== 'default') (function(key) { __webpack_require__.d(__webpack_exports__, key, function() { return _App_fs__WEBPACK_IMPORTED_MODULE_0__[key]; }) }(__WEBPACK_IMPORT_KEY__));\n\n\n//# sourceURL=webpack:///./src/browsershell.fsproj?");

/***/ })

/******/ });