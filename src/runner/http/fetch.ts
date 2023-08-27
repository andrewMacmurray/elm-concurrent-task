import { Request, Response, HttpError } from "./index";

export function http(request: Request): Promise<Response> {
  let controller;

  if (request.timeout) {
    controller = new AbortController();
    setTimeout(() => controller.abort(), request.timeout);
  }

  return fetch(request.url, {
    method: request.method,
    body: request.body || null,
    headers: new Headers(request.headers),
    signal: controller?.signal,
  })
    .then((res) => {
      const headers = Object.fromEntries(res.headers.entries());
      switch (request.expect) {
        case "JSON": {
          return res
            .json()
            .then((x) => ({
              url: res.url,
              headers: headers,
              status: res.status,
              statusText: res.statusText,
              body: x,
            }))
            .catch((e) => {
              return {
                error: {
                  reason: "BAD_BODY",
                  message: e.message,
                },
              };
            });
        }
        case "STRING": {
          return res.text().then((x) => ({
            url: res.url,
            headers: headers,
            status: res.status,
            statusText: res.statusText,
            body: x,
          }));
        }
        case "WHATEVER": {
          return {
            url: res.url,
            headers: headers,
            status: res.status,
            statusText: res.statusText,
            body: null,
          };
        }
      }
    })
    .catch((e) => {
      return {
        error: {
          reason: toHttpError(e),
          message: e.message,
        },
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
