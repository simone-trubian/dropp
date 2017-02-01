package dropp

import (
	"fmt"
	"log"
	"net/http"
	"regexp"
	"strconv"
	"time"

	gq "github.com/PuerkitoBio/goquery"
	gae "google.golang.org/appengine"
	db "google.golang.org/appengine/datastore"
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
	Active    bool
}

// Snapshot contains a snapshot of the current status of an item.
type Snapshot struct {
	Avaliabilty string
	AvCount     int
	OnEbay      bool
	CreatedAt   time.Time
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
		//Items:       make([]Item, 10, 10),
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
	http.Handle("/add", api.registerMiddlewares(api.dummyPost))
	http.Handle("/bg", api.registerMiddlewares(api.fetchBGAva))
}

func newAPI() *API {
	return &API{}
}

func (a *API) dummyPost(w http.ResponseWriter, r *http.Request) {
	ctx := gae.NewContext(r)
	item := &Item{
		SourceURL: "blalbalba.com",
		EbayID:    "10003420",
		ItemName:  "Dummy Item",
	}
	key := db.NewIncompleteKey(ctx, "Item", nil)
	_, err := db.Put(ctx, key, item)
	if err != nil {
		panic(err.Error())
	}
	http.Redirect(w, r, "/", http.StatusFound)
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

func (a *API) fetchBGAva(w http.ResponseWriter, r *http.Request) {

	// Fetch the page
	ctx := gae.NewContext(r)
	client := ufe.Client(ctx)
	resp, err := client.Get("http://eu.banggood.com/Wholesale-Warehouse-12V-4CH-Channel-315Mhz-Wireless-Remote-Control-Switch-wp-Eu-960985.html")
	if err != nil {
		panic(err.Error())
	}

	snap := &Snapshot{}
	snap.OnEbay = false
	snap.CreatedAt = time.Now()
	snap.getBGAva(resp)
	key := db.NewIncompleteKey(ctx, "Snapshot", nil)
	_, err = db.Put(ctx, key, snap)
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
	switch ava {
	case "Currently out of stock":
		snap.Avaliabilty = "Out"
		snap.AvCount = 0
		log.Print("out")
		return
	case "In stock, usually dispatched in 1 business day":
		snap.Avaliabilty = "Available"
		log.Print("available")
		return
	}
	rx, _ := regexp.Compile("[0-9]+")
	avaCount, _ := strconv.Atoi(rx.FindStringSubmatch(ava)[0])
	if avaCount <= 5 {
		snap.Avaliabilty = "Low"
		snap.AvCount = avaCount
		log.Print("low")
		return
	}
	snap.Avaliabilty = "Available"
	snap.AvCount = avaCount
	log.Print("available with number")
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
