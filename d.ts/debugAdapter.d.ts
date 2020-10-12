
import { DebugProtocol } from './debugProtocol'

export declare module DebugAdapter {

  // return indicates whether the message type was known (only true for 'request')
  function _vsc_handleJson(json: string): boolean;

  // return indicates whether the request command was known
  function _vsc_dispatchRequest(request: DebugProtocol.Request): boolean;

  // return indicates whether the response was successfully sent
  function sendResponse(response: DebugProtocol.Response): boolean;

  function prepareResponse(request: DebugProtocol.Request): DebugProtocol.Response;

  // if present, the return value indicates whether the corresponding response was successfully sent
  function genericRequest(
    response: DebugProtocol.Response,
    args: {[key: string]: any},
    request: DebugProtocol.Request
  ): boolean|void;

  function makeEvent(eventType: string, body: any): DebugProtocol.Event;

  // return indicates whether the event was successfully sent
  function sendEvent(event: DebugProtocol.Event): boolean;
}
