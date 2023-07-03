import axios, { AxiosError } from "axios";
import { Request, Response, HttpError, toHeaders } from "./index";

export function http(request: Request): Promise<Response> {
  return axios
    .request({
      method: request.method,
      url: request.url,
      data: request.body,
      headers: toHeaders(request),
    })
    .then((response) => {
      return {
        body: response.data,
        status: response.status,
        statusText: response.statusText,
      };
    })
    .catch((err) => {
      return {
        error: toHttpError(err),
        body: err.response?.data,
        status: err.response?.status,
        statusText: err.response?.statusText,
      };
    });
}

function toHttpError(err: AxiosError): HttpError {
  switch (err.code) {
    case "ECONNABORTED":
      return "TIMEOUT";
    case "ERR_NETWORK":
      return "NETWORK_ERROR";
    case "ECONNREFUSED":
      return "NETWORK_ERROR";
    case "ECONNRESET":
      return "NETWORK_ERROR";
    case "ERR_INVALID_URL":
      return "BAD_URL";
    default:
      return err.code || "";
  }
}
