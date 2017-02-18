package dropp

import (
	"bytes"
	"encoding/csv"
	"fmt"
	"log"
	"net/http"
	"strconv"
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
	Availability AvaComp
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
	LastUpdate string
	SnapDiffs  []SnapshotDiff
}

// SnapshotDiff Is created if there is a difference between the current and the
// previous snaphot.
type SnapshotDiff struct {
	ItemName       string
	ItemURL        string
	PreviousAva    AvaComp
	PreviousStatus bool
	PreviousPrice  float64
	CurrentAva     AvaComp
	CurrentStatus  bool
	CurrentPrice   float64
}

func (a *API) newEmailData(r *http.Request) EmailData {

	var (
		items            []Item
		snapshotDiffs    []SnapshotDiff
		currentSnapshots []Snapshot
		newDiff          SnapshotDiff
	)

	ctx := gae.NewContext(r)

	_, err := db.NewQuery("Item").GetAll(ctx, &items)
	for _, item := range items {

		currentSnapshots = []Snapshot{}
		// Get last two snaphots
		itemKey := db.NewKey(ctx, "Item", item.SourceURL, 0, nil)
		_, err =
			db.
				NewQuery("Snapshot").
				Ancestor(itemKey).
				Order("-CreatedAt").
				Limit(2).
				GetAll(ctx, &currentSnapshots)
		if err != nil {
			panic(err.Error())
		}

		// If item does not have 2 snaphots (it was just added) skip it
		if len(currentSnapshots) < 2 {
			continue
		}

		// Check if a new diff is needed
		//
		// ATTENTION!!!!!
		// As the qurey is ordered in descending order the first element of the
		// list is the MOST RECENT!!!!
		if currentSnapshots[0].Availability != currentSnapshots[1].Availability ||
			currentSnapshots[0].OnEbay != currentSnapshots[1].OnEbay ||
			currentSnapshots[0].Price != currentSnapshots[1].Price {

			newDiff = SnapshotDiff{
				ItemName:       item.ItemName,
				ItemURL:        item.SourceURL,
				PreviousAva:    currentSnapshots[1].Availability,
				CurrentAva:     currentSnapshots[0].Availability,
				PreviousStatus: currentSnapshots[1].OnEbay,
				CurrentStatus:  currentSnapshots[0].OnEbay,
				PreviousPrice:  currentSnapshots[1].Price,
				CurrentPrice:   currentSnapshots[0].Price,
			}

			snapshotDiffs = append(snapshotDiffs, newDiff)

		}
	}
	if err != nil {
		panic(err.Error())
	}

	return EmailData{
		LastUpdate: time.Now().Format("02/01/2006 - 15:04"),
		SnapDiffs:  snapshotDiffs,
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
	http.Handle("/upload_csv", api.registerMiddlewares(api.uploadCSV))
	http.Handle("/snapshot", http.HandlerFunc(api.snapshot)) // FIXME registerMiddlewares
	http.Handle(
		"/create_snapshots_task",
		api.recoverMiddleware(http.HandlerFunc(api.createSnapshotsTasks)))
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

	t, err := tmpl.
		New("email.html").
		Funcs(tmpl.FuncMap{"avaCol": AvaColor}).
		ParseFiles("templates/email.html")
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
	items := make([]Item, 0, 10) // FIXME try using an array instead of a slice
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

func (a *API) uploadCSV(w http.ResponseWriter, r *http.Request) {
	var (
		item    Item
		itemKey *db.Key
		items   []Item
		keys    []*db.Key
	)
	ctx := gae.NewContext(r)
	err := r.ParseMultipartForm(32 << 20)
	if err != nil {
		panic(err.Error())
	}

	file, _, err := r.FormFile("uploadfile")
	defer file.Close()
	if err != nil {
		panic(err.Error())
	}

	csvFileContent, err := csv.NewReader(file).ReadAll()
	if err != nil {
		panic(err.Error())
	}

	for _, row := range csvFileContent {

		// Create Item and append it to the array
		item = Item{
			ItemName:  row[0],
			SourceURL: row[1],
			EbayID:    row[2],
		}
		if isActive, err := strconv.ParseBool(row[3]); err == nil {
			item.IsActive = isActive
		} else {
			item.IsActive = false
		}
		items = append(items, item)

		// Create item key
		itemKey = db.NewKey(ctx, "Item", item.SourceURL, 0, nil)
		keys = append(keys, itemKey)
	}

	_, err = db.PutMulti(ctx, keys, items)
	if err != nil {
		panic(err.Error())
	}
	http.Redirect(w, r, "/", http.StatusFound)
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

	t, err := tmpl.
		New("email.html").
		Funcs(tmpl.FuncMap{"avaCol": AvaColor}).
		ParseFiles("templates/email.html")
	if err != nil {
		panic(err.Error())
	}

	err = t.Execute(&body, a.newEmailData(r))
	if err != nil {
		log.Printf("Error in executing template: %s", err)
		return
	}

	mgs := &mail.Message{
		Sender:   "Dropp <dropp@dropp-platform.appspotmail.com>",
		To:       []string{"simone.trubian@gmail.com", "stoxx84@gmail.com"},
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
	_, err := db.
		NewQuery("Item").
		Filter("isActive", true).
		GetAll(ctx, &items)

	for _, item := range items {
		log.Printf("Updating snapshot for item %s", item.SourceURL)
		client := ufe.Client(ctx)
		resp, err := client.Get(item.SourceURL)
		if err != nil {
			continue
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
	snap.Availability = NewAva(ava)
	return
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

/*
	Auth'n'Auth token Token for: import007 Expires: Thu, 09 Aug 2018 21:22:41 GMT

	AgAAAA**AQAAAA**aAAAAA**ocakWA**nY+sHZ2PrBmdj6wVnY+sEZ2PrA2dj6ABl4CoCpaBoAudj6x9nY+seQ**4psDAA**AAMAAA**9FXhu4fcHMlyizbBrdGsNW9gzm6ZdlUsUoMMp3J8iZ48RfjlWmcdSvZQfJCI7Redg5arrTahJZnBfoAAkJRqCXeoq4rP+0KUv+sd11xRmmUjWKkhzvHF4I9x2SGTnm58Px67+qTEUkA77BOGexdLiO0AE/2WdQYDrPu4vFfqxffNsEF1HF/LBGAb8qC9eAn0yfcsXReR7P5dHMX7rgjMq9QLJWrwDrtyQzc60KED/KCC3LygTpEvVC+Ki0vBXEa1E3fBzIq4sLyj2MTN89YwBo9ttlrbLJcQyJSwElwPWn1mffQTvzAoXXafGZeWHeyjVLTjcdWaCqjl8HyxFSNyG2JvMYIEqkwjeFtF8ZRNVIDy9/tiY1Mu4nEkq2bOQSGXawOMLRPoQVM98q/OpoSFXpSsNYn+syNKJINdoxCuhsODCleo4jIcOb3R37roMi4JR6QXSN5dtDNMJM7uzlFTKhV/nwZXD2Bfeih9nLRndDJ6xawiuWgI5Ok1SwW+2SWNPvi6EbY/OIWdUbGgvVT9h7xYkWWFoE2+e+5KeKxyWrVdeK6S1aGmwEYlxD8B8NA0mijWFFkSLyiXDP/0cowjKVSdu1GqYuirGMDN4zabtJyGNkOfyR9/u4jwCcd5NhLQ+57pI4jSeagFSFb7+M3Blohk9FvhIMCc1szwcWq7VpUEvnFAPrZ7UpbrBFBawi2HLVK221exmxZ38YgP1AWvY6+nWjkPCaU15Y+qFcXQ3YTeZUYPLHVctWFHq8BuUded
*/
