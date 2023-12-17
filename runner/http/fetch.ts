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
                body: x || null,
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

    case "UND_ERR":
      return "NETWORK_ERROR";
    case "UND_ERR_CONNECT_TIMEOUT":
      return "NETWORK_ERROR";
    case "UND_ERR_HEADERS_TIMEOUT":
      return "NETWORK_ERROR";
    case "UND_ERR_HEADERS_OVERFLOW":
      return "NETWORK_ERROR";
    case "UND_ERR_BODY_TIMEOUT":
      return "NETWORK_ERROR";
    case "UND_ERR_RESPONSE_STATUS_CODE":
      return "NETWORK_ERROR";
    case "UND_ERR_INVALID_ARG":
      return "NETWORK_ERROR";
    case "UND_ERR_INVALID_RETURN_VALUE":
      return "NETWORK_ERROR";
    case "UND_ERR_ABORTED":
      return "NETWORK_ERROR";
    case "UND_ERR_DESTROYED":
      return "NETWORK_ERROR";
    case "UND_ERR_CLOSED":
      return "NETWORK_ERROR";
    case "UND_ERR_SOCKET":
      return "NETWORK_ERROR";
    case "UND_ERR_NOT_SUPPORTED":
      return "NETWORK_ERROR";
    case "UND_ERR_REQ_CONTENT_LENGTH_MISMATCH":
      return "NETWORK_ERROR";
    case "UND_ERR_RES_CONTENT_LENGTH_MISMATCH":
      return "NETWORK_ERROR";
    case "UND_ERR_INFO":
      return "NETWORK_ERROR";
    case "UND_ERR_RES_EXCEEDED_MAX_SIZE":
      return "NETWORK_ERROR";
  }

  switch (err.name) {
    case "AbortError":
      return "TIMEOUT";
  }

  console.warn(
    `Unknown Http fetch error, consider submitting a PR adding an explicit case for this
    https://github.com/andrewMacmurray/elm-concurrent-task/blob/main/runner/http/fetch.ts#L60
    `,
    err
  );
  return "NETWORK_ERROR";
}
