
import { DebugProtocol } from './debugProtocol'

export declare module DebugAdapter {

  function _vsc_dispatchRequest(request: DebugProtocol.Request): void;

  function sendResponse(response: DebugProtocol.Response): void;

  function prepareResponse(request: DebugProtocol.Request): DebugProtocol.Response;

  function genericRequest(
    response: DebugProtocol.Response,
    args: {[key: string]: any},
    request: DebugProtocol.Request
  ): void;
}
