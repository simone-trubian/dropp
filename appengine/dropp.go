package dropp

import (
	"fmt"
	gae "google.golang.org/appengine"
	usr "google.golang.org/appengine/user"
	"net/http"
)

// Item models items sold by Dropp
type Item struct {
	SourceURL string
	EbayURL   string
	ItemName  string
}

func init() {
	http.HandleFunc("/", handler)
}

func handler(w http.ResponseWriter, r *http.Request) {
	ctx := gae.NewContext(r)
	w.Header().Set("Content-type", "text/html; charset=utf-8")

	currentUser := usr.Current(ctx)
	if currentUser == nil {
		url, _ := usr.LoginURL(ctx, "/")
		fmt.Fprintf(w, `<a href="%s">Sign in or register</a>`, url)
		return
	} else {
		url, _ := usr.LogoutURL(ctx, "/")
		fmt.Fprintf(
			w,
			`Welcome, %s! (<a href="%s">sign out</a>)`,
			currentUser,
			url)
	}
}
