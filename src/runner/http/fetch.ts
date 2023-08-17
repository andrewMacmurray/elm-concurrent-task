import { Request, Response, HttpError, toHeaders } from "./index";

export function http(request: Request): Promise<Response> {
  return fetch(request.url, {
    method: request.method,
    body: request.body ? JSON.stringify(request.body) : null,
    headers: toHeaders(request),
  })
    .then((res) => {
      switch (request.expect) {
        case "JSON": {
          return res.json().then((x) => ({
            status: res.status,
            statusText: res.statusText,
            body: x,
          }));
        }
        case "STRING": {
          return res.text().then((x) => ({
            status: res.status,
            statusText: res.statusText,
            body: x,
          }));
        }
        case "WHATEVER": {
          return {
            status: res.status,
            statusText: res.statusText,
            body: null,
          };
        }
      }
    })
    .catch((e) => {
      return {
        error: toHttpError(e),
      };
    });
}

function toHttpError(err): HttpError {
  switch (err.cause?.code) {
    case "ENOTFOUND":
      return "NETWORK_ERROR";
    case "ECONNREFUSED":
      return "NETWORK_ERROR";
    case "ECONNRESET":
      return "NETWORK_ERROR";
    case "EAGAIN":
      return "NETWORK_ERROR";
    case "ERR_INVALID_URL":
      return "BAD_URL";
    default:
      return err.cause?.code;
  }
}
