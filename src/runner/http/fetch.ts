import { Request, Response, HttpError, toHeaders } from "./index";

export function http(request: Request): Promise<Response> {
  let controller;

  if (request.timeout) {
    controller = new AbortController();
    setTimeout(() => controller.abort(), request.timeout);
  }

  return fetch(request.url, {
    method: request.method,
    body: request.body || null,
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
        body: e.message,
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
  }

  switch (err.name) {
    case "AbortError":
      return "TIMEOUT";
    case "TypeError":
      return "BAD_BODY";
  }

  return err.cause?.code;
}
