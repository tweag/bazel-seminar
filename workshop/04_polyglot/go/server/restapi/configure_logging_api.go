// This file is safe to edit. Once it exists it will not be overwritten

package restapi

import (
	"crypto/tls"
	"net/http"
	"sync"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/runtime"
	"github.com/go-openapi/runtime/middleware"

	"polyglot.com/server/restapi/operations"
)

/* **************************************** */
/* Simple implementation of the logging API */
/* **************************************** */

var logEntries []string

var entriesLock = &sync.Mutex{}

func addEntry(entry string) int64 {
	entriesLock.Lock()
	defer entriesLock.Unlock()

	logEntries = append(logEntries, entry)

	return int64(len(logEntries))
}

func Min(x, y int) int {
	if x > y {
		return y
	}
	return x
}

func allEntries(limit int) []string {
	if limit < 0 {
		return logEntries
	} else {
		return logEntries[:Min(len(logEntries), limit)]
	}
}

//go:generate swagger generate server --target ../../server --name LoggingAPI --spec ../../../../../.cache/bazel/_bazel_jovyan/59f58955bf3a2725b9ec1cc132ed5f2c/execroot/polyglot/bazel-out/k8-fastbuild/bin/go/server.runfiles/polyglot/swagger/swagger.json --principal interface{}

func configureFlags(api *operations.LoggingAPIAPI) {
	// api.CommandLineOptionsGroups = []swag.CommandLineOptionsGroup{ ... }
}

func configureAPI(api *operations.LoggingAPIAPI) http.Handler {
	// configure the api here
	api.ServeError = errors.ServeError

	// Set your custom logger if needed. Default one is log.Printf
	// Expected interface func(string, ...interface{})
	//
	// Example:
	// api.Logger = log.Printf

	api.UseSwaggerUI()
	// To continue using redoc as your UI, uncomment the following line
	// api.UseRedoc()

	api.JSONConsumer = runtime.JSONConsumer()

	api.JSONProducer = runtime.JSONProducer()

	api.GetAPILogsHandler = operations.GetAPILogsHandlerFunc(func(params operations.GetAPILogsParams) middleware.Responder {
		var limit int
		if params.Limit != nil {
			limit = int(*params.Limit)
		} else {
			limit = -1
		}
		return operations.NewGetAPILogsOK().WithPayload(allEntries(limit))
	})
	api.PostAPILogsMessageHandler = operations.PostAPILogsMessageHandlerFunc(func(params operations.PostAPILogsMessageParams) middleware.Responder {
		return operations.NewPostAPILogsMessageOK().WithPayload(addEntry(params.Message))
	})

	api.PreServerShutdown = func() {}

	api.ServerShutdown = func() {}

	return setupGlobalMiddleware(api.Serve(setupMiddlewares))
}

// The TLS configuration before HTTPS server starts.
func configureTLS(tlsConfig *tls.Config) {
	// Make all necessary changes to the TLS configuration here.
}

// As soon as server is initialized but not run yet, this function will be called.
// If you need to modify a config, store server instance to stop it individually later, this is the place.
// This function can be called multiple times, depending on the number of serving schemes.
// scheme value will be set accordingly: "http", "https" or "unix".
func configureServer(s *http.Server, scheme, addr string) {
}

// The middleware configuration is for the handler executors. These do not apply to the swagger.json document.
// The middleware executes after routing but before authentication, binding and validation.
func setupMiddlewares(handler http.Handler) http.Handler {
	return handler
}

// The middleware configuration happens before anything, this middleware also applies to serving the swagger.json document.
// So this is a good place to plug in a panic handling middleware, logging and metrics.
func setupGlobalMiddleware(handler http.Handler) http.Handler {
	return handler
}
