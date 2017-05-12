package dropp

import (
	"bytes"
	"encoding/csv"
	"log"
	"net/http"
	"strconv"
	"time"

	pst "github.com/sethgrid/pester"
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
	DataURL   string
	EbayID    string
	ItemName  string
	CreatedAt time.Time
	UpdatedAt time.Time
	IsActive  bool
}

// HomeData contains all data needed by the home page template
type HomeData struct {
	CurrentUser *usr.User
	LogoutURL   string
	ItemCount   int
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
				ItemName:        item.ItemName,
				ItemURL:         item.SourceURL,
				EbayID:          item.EbayID,
				PreviousAva:     currentSnapshots[1].Availability,
				PreviousAvaComp: NewAva(currentSnapshots[1].Availability),
				CurrentAva:      currentSnapshots[0].Availability,
				CurrentAvaComp:  NewAva(currentSnapshots[0].Availability),
				PreviousStatus:  currentSnapshots[1].OnEbay,
				CurrentStatus:   currentSnapshots[0].OnEbay,
				PreviousPrice:   currentSnapshots[1].Price,
				CurrentPrice:    currentSnapshots[0].Price,
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

func newAPI() *API {
	return &API{}
}

func (a *API) snapshot(w http.ResponseWriter, r *http.Request) {
	emailData := a.newEmailData(r)

	t, err := getEmailTemplate()

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
	itemCount, err := db.NewQuery("Item").Count(ctx)
	homeData.ItemCount = itemCount

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
	if err != nil {
		panic(err.Error())
	}
	defer func() {
		err = file.Close()
		if err != nil {
			log.Printf("Error while trying to close CSV file: %s", err)
			panic(err.Error())
		}
	}()

	csvFileContent, err := csv.NewReader(file).ReadAll()
	if err != nil {
		panic(err.Error())
	}

	for _, row := range csvFileContent {

		isActive, err := strconv.ParseBool(row[3])
		if err != nil {
			log.Printf("Error while trying to load row %s, error: %s", row[0], err)
			panic(err.Error())
		}

		// Create Item and append it to the array
		item = Item{
			ItemName:  row[0],
			SourceURL: row[1],
			EbayID:    row[2],
			IsActive:  isActive,
			DataURL:   row[4],
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

	t, err := getEmailTemplate()
	if err != nil {
		log.Printf("Error in generating template: %s", err)
		return
	}

	err = t.Execute(&body, a.newEmailData(r))
	if err != nil {
		log.Printf("Error in executing template: %s", err)
		return
	}

	mgs := &mail.Message{
		Sender:   conf.EmailSender,
		To:       conf.EmailRecipients,
		Subject:  conf.EmailSubject,
		HTMLBody: body.String(),
	}

	err = mail.Send(ctx, mgs)
	if err != nil {
		panic(err.Error())
	}
}

func (a *API) createSnapshots(w http.ResponseWriter, r *http.Request) {

	ebayServURL := conf.EbayServURL

	// Fetch the page
	ctx := gae.NewContext(r)
	items := make([]Item, 0)
	_, err := db.
		NewQuery("Item").
		Filter("IsActive =", true).
		GetAll(ctx, &items)
	if err != nil { // If the DB doesn't return a list of items fail right away
		panic(err.Error())
	}

	client := pst.NewExtendedClient(ufe.Client(ctx))
	client.Concurrency = 3
	client.MaxRetries = 1
	client.Backoff = pst.ExponentialJitterBackoff
	client.KeepLog = true

	for _, item := range items {
		log.Printf("Updating snapshot for item %s", item.SourceURL)
		itemKey := db.NewKey(ctx, "Item", item.SourceURL, 0, nil)
		snap := &Snapshot{}
		snap.CreatedAt = time.Now()

		// Fetch BangGood data
		sourceResp, err := client.Get(item.DataURL)
		if err != nil {
			log.Printf("Error while fetching item data %s", client.LogString())
		} else {
			snap.getSourceData(sourceResp)
		}

		defer func() {
			log.Printf("Closing BG response body %p", sourceResp)
			err := sourceResp.Body.Close()
			if err != nil {
				log.Printf("Error while closing BG page %s", err)
			}
		}()

		// Fetch Ebay status
		ebayResp, err := client.Get(ebayServURL + item.EbayID)
		if err != nil {
			log.Printf("Error while fetching Ebay item status %s", err)
		} else {
			snap.getEbayStatus(ebayResp)
		}

		defer func(ebayResp *http.Response) {
			log.Printf("Closing Ebay response body %p", ebayResp)
			err := ebayResp.Body.Close()
			if err != nil {
				log.Printf("Error while closing Ebay service response %s", client.LogString())
			}
		}(ebayResp)

		// Create snapshot
		snapKey := db.NewIncompleteKey(ctx, "Snapshot", itemKey)
		_, err = db.Put(ctx, snapKey, snap)
	}
}

func getEmailTemplate() (*tmpl.Template, error) {
	emailFunctions := tmpl.FuncMap{
		"avaCol":  AvaColor,
		"ebayCol": EbayStatusColor,
	}

	return tmpl.
		New("email.html").
		Funcs(emailFunctions).
		ParseFiles("templates/email.html")
}

/*
	package main

import (
	"fmt"
	"time"
)

func add1(s []int, d time.Duration, c chan int, done chan bool) {
	for _, v := range s {
		time.Sleep(d * time.Second)
		c <- v + 1
		done <- false
	}
	done <- true
}

func main() {
	var p1 []int
	var p2 []int

	s := []int{1, 1, 1, 1, 1, 1}

	c := make(chan int, len(s))
	done1 := make(chan bool)
	done1 := make(chan bool)
	go add1(s,1, c, done1)
	go add1(s,1, c, done2)

	for !(<- done2) {
		p = append(p, <- c)
	}

	for !(<- done2) {
		p = append(p, <- c)
	}

	fmt.Println(p)
}
*/
