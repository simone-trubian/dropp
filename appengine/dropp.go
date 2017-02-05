package dropp

import (
	"bytes"
	//"context"
	"fmt"
	"log"
	"net/http"
	"regexp"
	"strconv"
	"strings"
	"time"

	gq "github.com/PuerkitoBio/goquery"
	gae "google.golang.org/appengine"
	db "google.golang.org/appengine/datastore"
	mail "google.golang.org/appengine/mail"
	tq "google.golang.org/appengine/taskqueue"
	ufe "google.golang.org/appengine/urlfetch"
	usr "google.golang.org/appengine/user"
	tmpl "html/template"
)

// API contains all data needed for the API
type API struct {
	CurrentUser *usr.User
	LogoutURL   string
}

// Item models items sold by Dropp
type Item struct {
	SourceURL string
	EbayID    string
	ItemName  string
	CreatedAt time.Time
	UpdatedAt time.Time
	IsActive  bool
}

// Snapshot contains a snapshot of the current status of an item.
type Snapshot struct {
	Availability string
	AvCount      int
	OnEbay       bool
	Price        float64
	CreatedAt    time.Time
}

// HomeData contains all data needed by the home page template
type HomeData struct {
	CurrentUser *usr.User
	LogoutURL   string
	Items       *[]Item
}

func (a *API) newHomeData() HomeData {
	return HomeData{
		CurrentUser: a.CurrentUser,
		LogoutURL:   a.LogoutURL,
	}
}

// EmailData contains all data needed by the availability report email
type EmailData struct {
	Timestamp string
	Snapshots *[]Snapshot
}

func (a *API) newEmailData(r *http.Request) EmailData {

	var (
		items     []Item
		snapshots []Snapshot
	)

	ctx := gae.NewContext(r)
	_, err := db.NewQuery("Item").GetAll(ctx, &items)
	for _, item := range items {
		itemKey := db.NewKey(ctx, "Item", item.SourceURL, 0, nil)
		_, err =
			db.
				NewQuery("Snapshot").
				Ancestor(itemKey).
				Limit(2).
				GetAll(ctx, &snapshots)
	}
	if err != nil {
		panic(err.Error())
	}

	return EmailData{
		Timestamp: time.Now().Format("02/01/2006 - 15:04"),
		Snapshots: &snapshots,
	}
}

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
	http.Handle("/item", api.registerMiddlewares(api.item))
	http.Handle("/snapshot", api.registerMiddlewares(api.snapshot))
	http.Handle(
		"/create_snapshots_task", api.registerMiddlewares(api.createSnapshotsTasks))
	http.Handle(
		"/create_snapshots",
		api.recoverMiddleware(http.HandlerFunc(api.createSnapshots)))
	http.Handle(
		"/send_report_email",
		api.recoverMiddleware(http.HandlerFunc(api.sendReportEmail)))
}

func newAPI() *API {
	return &API{}
}

func (a *API) item(w http.ResponseWriter, r *http.Request) {
	ctx := gae.NewContext(r)

	item := &Item{
		SourceURL: r.FormValue("item-url-input"),
		EbayID:    r.FormValue("item-ebay-id-input"),
		ItemName:  r.FormValue("item-name-input"),
		CreatedAt: time.Now(),
		UpdatedAt: time.Now(),
	}

	isActiveInput := r.FormValue("item-is-active-input")
	if isActiveInput == "yes" {
		item.IsActive = true
	} else {
		item.IsActive = false
	}

	key := db.NewKey(ctx, "Item", r.FormValue("item-source-name"), 0, nil)
	_, err := db.Put(ctx, key, item)
	if err != nil {
		panic(err.Error())
	}
	http.Redirect(w, r, "/", http.StatusFound)
}

func (a *API) snapshot(w http.ResponseWriter, r *http.Request) {
	emailData := a.newEmailData(r)

	t, err := tmpl.ParseFiles("templates/email.html")
	if err != nil {
		panic(err.Error())
	}
	err = t.Execute(w, emailData)
	if err != nil {
		panic(err.Error())
	}
}

func (a *API) homePage(w http.ResponseWriter, r *http.Request) {
	ctx := gae.NewContext(r)
	homeData := a.newHomeData()
	items := make([]Item, 0, 10)
	_, err := db.NewQuery("Item").GetAll(ctx, &items)
	homeData.Items = &items

	t, err := tmpl.ParseFiles("templates/home.html")
	if err != nil {
		panic(err.Error())
	}
	err = t.Execute(w, homeData)
	if err != nil {
		panic(err.Error())
	}
}

func (a *API) createSnapshotsTasks(w http.ResponseWriter, r *http.Request) {
	ctx := gae.NewContext(r)

	createSnapshotsTask := tq.NewPOSTTask(
		"/create_snapshots", map[string][]string{})

	sendReportEmailTask := tq.NewPOSTTask(
		"/send_report_email", map[string][]string{})

	allTasks := []*tq.Task{createSnapshotsTask, sendReportEmailTask}
	_, err := tq.AddMulti(ctx, allTasks, "snapshot")

	if err != nil {
		log.Printf("Error while trying to add task: %s", err)
	} else {
		log.Printf("Starting new update snapshot task %s", createSnapshotsTask.Name)
	}

	if err != nil {
		panic(err.Error())
	}
	http.Redirect(w, r, "/", http.StatusFound)
}

func (a *API) sendReportEmail(w http.ResponseWriter, r *http.Request) {
	ctx := gae.NewContext(r)

	var body bytes.Buffer

	t, err := tmpl.ParseFiles("templates/email.html")
	if err != nil {
		panic(err.Error())
	}

	err = t.Execute(&body, a.newEmailData(r))
	if err != nil {
		log.Printf("Error in executing template: %s", err)
		return
	}

	mgs := &mail.Message{
		Sender:   "Dropp <dropp@dropp-platform.appspot.com>",
		To:       []string{"simone.trubian@gmail.com"},
		Subject:  "Daily Snapshot Report",
		HTMLBody: body.String(),
	}

	err = mail.Send(ctx, mgs)
	if err != nil {
		panic(err.Error())
	}
}

func (a *API) createSnapshots(w http.ResponseWriter, r *http.Request) {

	// Fetch the page
	ctx := gae.NewContext(r)
	items := make([]Item, 0, 10)
	_, err := db.NewQuery("Item").GetAll(ctx, &items)
	for _, item := range items {
		log.Printf("Updating snapshot for item %s", item.SourceURL)
		client := ufe.Client(ctx)
		resp, err := client.Get(item.SourceURL)
		if err != nil {
			panic(err.Error())
		}

		itemKey := db.NewKey(ctx, "Item", item.SourceURL, 0, nil)
		snap := &Snapshot{}
		snap.OnEbay = false
		snap.CreatedAt = time.Now()
		snap.getBGAva(resp)
		snapKey := db.NewIncompleteKey(ctx, "Snapshot", itemKey)
		_, err = db.Put(ctx, snapKey, snap)
	}
	if err != nil {
		panic(err.Error())
	}
}

func (snap *Snapshot) getBGAva(response *http.Response) {
	// Scrape the page and get availability
	doc, err := gq.NewDocumentFromResponse(response)
	if err != nil {
		panic(err.Error())
	}
	ava := doc.Find(".status").Text()
	switch {
	case ava == "Currently out of stock":
		snap.Availability = "Out"
		snap.AvCount = 0
		log.Print("out")
		return
	case ava == "In stock, usually dispatched in 1 business day":
		snap.Availability = "Available"
		log.Print("available")
		return
	case strings.Contains(ava, "usually dispatched in 1 business day"):
		rx, _ := regexp.Compile("[0-9]+")
		avaCount, _ := strconv.Atoi(rx.FindStringSubmatch(ava)[0])
		if avaCount <= 5 {
			snap.Availability = "Low"
			snap.AvCount = avaCount
			log.Print("low")
			return
		}
		snap.Availability = "Available"
		snap.AvCount = avaCount
		log.Print("available with number")
		return
	default:
		snap.Availability = "Could not fetch update"
		snap.AvCount = 0
		log.Printf("Could not fetch update: %s", ava)
		return
	}
}

func (a *API) registerMiddlewares(
	handFunc func(w http.ResponseWriter, r *http.Request)) http.Handler {
	return a.recoverMiddleware(a.authMiddleware(http.HandlerFunc(handFunc)))
}

func (a *API) recoverMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		defer func() {
			rec := recover()
			if rec != nil {
				http.Error(w, rec.(string), http.StatusInternalServerError)
			}
		}()
		next.ServeHTTP(w, r)
	})
}

func (a *API) authMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-type", "text/html; charset=utf-8")
		ctx := gae.NewContext(r)
		currentUser := usr.Current(ctx)

		// The user is not signed in, generate a sign-in URL
		if currentUser == nil {
			url, err := usr.LoginURL(ctx, "/")
			if err != nil {
				panic(err.Error())
			}
			fmt.Fprintf(w, `<a href="%s">Sign in or register</a>`, url)
			return
		}

		// The user is signed but not allowed to access the platform, ask them
		// to log out and log in as another user, generate a sign-out URL
		if !allowedUsers[currentUser.Email] {
			url, err := usr.LogoutURL(ctx, "/")
			if err != nil {
				panic(err.Error())
			}
			fmt.Fprintf(
				w,
				`This user is not allowed in the platform, <a href="%s">sign in</a> as a different user`,
				url)
			return
		}

		// The user is signed in and allowed, serve the page and generate a
		// sign-out URL
		if allowedUsers[currentUser.Email] {
			url, err := usr.LogoutURL(ctx, "/")
			if err != nil {
				panic(err.Error())
			}
			a.CurrentUser = currentUser
			a.LogoutURL = url
		}

		next.ServeHTTP(w, r)
	})
}
