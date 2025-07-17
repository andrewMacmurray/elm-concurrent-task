import { HttpResponse, HttpRequest, HttpError } from "./index.js";

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
      const headers: { [key: string]: string } = {};
      res.headers.forEach((val, key) => {
        headers[key] = val;
      });

      switch (request.expect) {
        case "STRING": {
          return res.text().then((x) => ({
            url: res.url,
            headers: headers,
            statusCode: res.status,
            statusText: res.statusText,
            body: x || "",
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
        case "BYTES": {
          return res
            .blob()
            .then((blob) => blob.text())
            .then((x) => {
              return {
                url: res.url,
                headers: headers,
                statusCode: res.status,
                statusText: res.statusText,
                body: x || "",
              };
            });
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
  if (err.name === "AbortError") {
    return "TIMEOUT";
  } else if (err.cause?.code === "ERR_INVALID_URL") {
    return "BAD_URL";
  } else {
    return "NETWORK_ERROR";
  }
}
