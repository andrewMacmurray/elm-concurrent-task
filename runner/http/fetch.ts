import { HttpResponse, HttpRequest, HttpError } from "./index";

export function http(request: HttpRequest): Promise<HttpResponse> {
  let controller: AbortController | undefined;

  if (request.timeout) {
    controller = new AbortController();
    setTimeout(() => controller?.abort(), request.timeout);
  }

  return fetch(request.url, {
    method: request.method,
    body: request.body || null,
    headers: new Headers(request.headers),
    signal: controller?.signal,
  })
    .then((res: Response) => {
      const headers = Object.fromEntries(res.headers.entries());
      switch (request.expect) {
        case "STRING": {
          return res.text().then((x) => ({
            url: res.url,
            headers: headers,
            statusCode: res.status,
            statusText: res.statusText,
            body: x || null,
          }));
        }
        case "JSON": {
          return res.text().then((x) => ({
            url: res.url,
            headers: headers,
            statusCode: res.status,
            statusText: res.statusText,
            body: x || null,
          }));
        }
        case "WHATEVER": {
          return {
            url: res.url,
            headers: headers,
            statusCode: res.status,
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

function toHttpError(err: any): HttpError {
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
