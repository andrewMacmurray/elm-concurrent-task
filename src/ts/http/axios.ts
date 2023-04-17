import axios, { AxiosError } from "axios";
import { Request, Response } from "./task";

export function driver(request: Request): Promise<Response> {
  return axios
    .request({
      method: request.method,
      url: request.url,
      data: request.body,
      headers: Object.fromEntries(
        request.headers.map((header) => [header.name, header.value])
      ),
    })
    .then((response) => ({
      body: response.data,
      status: response.status,
      statusText: response.statusText,
    }))
    .catch((err) => ({
      error: toHttpError(err),
      body: err.response?.data,
      status: err.response?.status,
      statusText: err.response?.statusText,
    }));
}

function toHttpError(err: AxiosError): HttpError {
  switch (err.code) {
    case "ECONNABORTED":
      return "TIMEOUT";
    case "ERR_NETWORK":
      return "NETWORK_ERROR";
    case "ERR_INVALID_URL":
      return "BAD_URL";
    default:
      return "UNKNOWN";
  }
}
