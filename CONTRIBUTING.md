
# Contributing

This project is still under development and all contributions are welcome :)

If you encounter bugs/problems while using the debugger or have suggestions for new features, please open an issue on github.

If you want to contribute code but are unsure how/where to integrate your code in the package, please don't hesitate to ask and I'll try to clarify the packet structure or write some better documentation (which is naturally on the todo list anyways...).

Current ideas/todos include:
* Improve display of variables (e.g. promises, R6 objects).
This is rather simple to integrate in defaultVarInfos.R, but might take some time to implement precisely and error-free.
* Improve handling of breakpoints in function definitions by `.vsc.debugSource`
* Improve handling of breakpoints in nested functions
* Implement proper user facing functions to add/modify VarInfos


# File Structure

Below is a rough grouping and explanation of the R files from this package.
In the directory `d.ts` are typescript declaration files with matching names for some of the R files.
These do not contain any running code, but are used to document the interfaces/classes used in the package.

## General
### communications.R 
This file contains the functions that handle communication with the debug client.
DAP requests are received (as json/text) on socketConnections by `.vsc.listenForDAP` and `.vsc.listenForJSON`.
These jsons are parsed by `.vsc.handleJson` and converted to named lists.

Responses and events are passed as named list to `sendToVsc`,
where they are converted to json-strings and sent via the corresponding socketConnection.

### debugAdapter.R
This file contains most of the "DAP-logic" and is the only file that contains calls to/is called by pretty much all other files.
`.vsc.dispatchRequest` is called by functions from communications.R with DAP requests represented as named lists.
Depending on the command of the request, the corresponding R functions `xxxRequest()` are called.

The called R functions return their results by calling `sendReponse`.
Events (e.g. output, breakpoints, ...) are reported by internal R functions to the debug client by calling `sendEvent.`
Most events are implemented in separates functions `sendXxxEvent()` and `makeXxxEvent()`.

### global.R
This file contains the definition of `session`, a global variable (an environment) that contains information about the configuration and state of the current debug session.
Most of the state of the debug session is represented by an R6 object of class `State`, which is updated when e.g. breakpoints are hit or code execution is started.

### launch.R
This file handles the launch sequence of the debug adapter.
This includes the requests `initialize`, `launch`/`attach`, and `configurationDone`.
The launch configuration is interpreted and stored to the `session`.

### flow.R
This file handles the commands used to control the flow of a debugged session.
This includes the requests `continue`, `next`, `stepIn`, `stepOut`, `disconnect`, `terminate`.
Furthermore, the custom request (not specified by the DAP!) `showingPrompt`
and the error handler `.vsc.onError` are defined here.

---

## Helpers
### frameManagement.R
This file contains functions that are used to keep track of internal stack frames,
in order to show the user only "their frames".
Most exported functions from this package call `registerEntryFrame` on entry and `unregisterEntryFrame` when they finish.
When user code is executed (e.g. in `eval` requests), `registerLaunchFrame` is called to register the new frames as "user frames".

`getTopFrameId`, `getSkipFromBottom`, and `getExternalFrames` can be used to retrieve the indices of the last/first/all user frames.

### ppid.R 
This file contains only the wrapper function `getPpid` which is used to retrieve the process id of R\'s parent process (usually the shell).

### utils.R
Contains some non DAP specific helper functions.

---

## Stack
### stackTree.R
This file contains the R6 classes that are used to internally represent the tree of stack frames, scopes, and variables of the current debug session.
Each of these objects is represented by an individual node class (`StackNode`, `FrameNode`, ...)
which inherit from a common base class `Node`.
The info stored in a node can be accessed through the functions `getChildren` and `getContent`.
The children of a node are the nodes representing the next items on the next level of the stack tree
(e.g. the children of a `FrameNode` are the `ScopeNodes`, the children of a `ScopeNode` are `VariableNodes`).
The content of a node usually extends the corresponding interface declared in the DAP
(e.g. `VariableNode$getcontent()` can be used directly as `DebugProtocol.Variable`)

A (typescript) declaration of these classes can be found in `stackTree.d.ts`.

### stackTreeHelpers.R
Contains helper functions used in stackTree.R.

### getStack.R
Contains handlers for the requests `stackTrace`, `scopes`, and `variables`.
Mostly just wrappers for calls to `getContent` and `getChildren`.

---

## Variables Info
Information retrieval for variables is done through `VarInfos` (see customVarInfo.d.ts).
These contain a function `doesApply`, which is used to determine if they should be used for a given variable,
and a number of functions (or fixed values) that return information about the variable.
To retrieve information about a given variable, each `VarInfo$doesApply()` is tested and if it returns `TRUE`, the items supplied by this VarInfo are used for the variable.
This is repeated with the remaining items until all requested information is found.

A VarInfo can contain all information about a specific type of variable
(e.g. `NULL` is quite easy to represent with a single VarInfo),
or add just a single piece of information for a broad range of variables
(e.g. the entry `R6` only yields a string for the variable type of R6 objects),
or anything in between.

### customVarInfo.R
Contains `.vsc.applyVarInfos` which is used to get information about individual variables,
which is used by functions from stackTree.R.

TODO/WIP:
Add user facing functions to add/edit the list of `VarInfos` used.
Some of these exist but need to be cleaned up.

### defaultVarInfo.R
Contains a list of default `VarInfos`.

### defaultVarInfoHelpers.R
Helpers for defaultVarInfo.R.

---

## Breakpoints
### breakpoints.R
Contains `setBreakpoints` which is used to set breakpoints in functions that are loaded in the workspace already.
This is done by using the functions `findLineNum` and `trace` to insert calls to `browser` in the function body.

### breakpointsHelpers.R
Contains the functions `.vsc.preDebugSourceBreakpoint` and `.vsc.preBreakpoint`, which are inserted in the function body to notify the debug client of the breakpoint.

### breakpointsManagement.R
Contains functions used to store/retrieve breakpoints.
Is especially important for breakpoints that are in functions which are not loaded yet.

---

## Eval
### debugSource.R 
Contains `.vsc.debugSource`, a heavily modified version of `source` which honours breakpoints from breakpointsManagement.R.
These breakpoints are set by a custom function which is very similar to a combination of `findLineNum` and `trace`.

### evaluate.R
Contains code used to handle `evaluate` requests.
The evaluated code is wrapped in mutliple layers of error handling, and output capturing.

---

## Others
### completion.R
Handles `completions` requests, sent while typing text in the debug console.

### setVar.R
Contains code used to set the value of variables in the variables window through `setVariable` requests.

### overwrites.R
Contains the functions `.vsc.print`, `.vsc...` used to overwrite `base::print` etc.

