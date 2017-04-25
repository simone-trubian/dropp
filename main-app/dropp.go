package dropp

import (
	"net/http"
)

// Allowed users
var allowedUsers map[string]bool
var api *API

func init() {
	allowedUsers = map[string]bool{
		"simone.trubian@gmail.com": true,
		"stoxx84@gmail.com":        true,
		"test@example.com":         true,
	}
	api = newAPI()
	http.Handle("/", api.registerMiddlewares(api.homePage))
	http.Handle("/upload_csv", api.registerMiddlewares(api.uploadCSV))
	http.Handle("/snapshot", http.HandlerFunc(api.snapshot)) // FIXME registerMiddlewares
	http.Handle(
		"/create_snapshots_task",
		api.recoverMiddleware(http.HandlerFunc(api.createSnapshotsTasks)))
	http.Handle(
		"/create_snapshots",
		http.HandlerFunc(api.createSnapshots))
	http.Handle(
		"/send_report_email",
		api.recoverMiddleware(http.HandlerFunc(api.sendReportEmail)))
}
