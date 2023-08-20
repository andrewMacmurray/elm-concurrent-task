import { Request, Response, HttpError, toHeaders } from "./index";

export function http(request: Request): Promise<Response> {
  let controller;

  if (request.timeout) {
    controller = new AbortController();
    setTimeout(() => controller.abort(), request.timeout);
  }

  return fetch(request.url, {
    method: request.method,
    body: request.body ? JSON.stringify(request.body) : null,
    headers: toHeaders(request),
    signal: controller?.signal,
  })
    .then((res) => {
      switch (request.expect) {
        case "JSON": {
          return res
            .json()
            .then((x) => ({
              status: res.status,
              statusText: res.statusText,
              body: x,
            }))
            .catch((e) => {
              return {
                status: res.status,
                statusText: res.statusText,
                error: "BAD_BODY",
                body: e,
              };
            });
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
  if (err.name === "AbortError") {
    return "TIMEOUT";
  }
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
