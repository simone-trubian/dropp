package dropp

import (
	"bytes"
	"encoding/json"
	"io/ioutil"
	"log"
	"net/http"
	"strconv"
	"time"

	gq "github.com/PuerkitoBio/goquery"
)

// Snapshot contains a snapshot of the current status of an item.
type Snapshot struct {
	Availability string
	OnEbay       EbayStatus
	Price        float64
	CreatedAt    time.Time
}

// SnapshotDiff Is created if there is a difference between the current and the
// previous snaphot.
type SnapshotDiff struct {
	ItemName        string
	ItemURL         string
	EbayID          string
	PreviousAva     string
	PreviousAvaComp AvaComp
	PreviousStatus  EbayStatus
	PreviousPrice   float64
	CurrentAva      string
	CurrentAvaComp  AvaComp
	CurrentStatus   EbayStatus
	CurrentPrice    float64
}

// BGData is a partial representation ot the data JSON for a Chinese BG item
type BGData struct {
	Message string  `json:"message"`
	Price   float64 `json:"final_price"`
}

// BGDataEuro is a partial representation ot the data JSON for a Euro BG item
type BGDataEuro struct {
	Message string `json:"message"`
	Price   string `json:"final_price"`
}

func (snap *Snapshot) getBGAva(response *http.Response) {
	// Scrape the page and get availability
	log.Print("Scraping BG page to retrieve availability")
	doc, err := gq.NewDocumentFromResponse(response)
	if err != nil {
		panic(err.Error())
	}
	ava := doc.Find(".status").Text()
	snap.Availability = ava
	return
}
func (snap *Snapshot) getSourceData(response *http.Response) {
	data := BGData{}
	dataEuro := BGDataEuro{}
	var respBody []byte

	// Copy body content in a buffer variable so that it can be re-read and
	// close the request body.
	respBody, err := ioutil.ReadAll(response.Body)
	response.Body = ioutil.NopCloser(bytes.NewBuffer(respBody))

	// Try unmarshalling a the response using the proper object.
	err = json.Unmarshal(respBody, &data)
	if err != nil {
		log.Printf("Error while decoding BG data for item %s: %s",
			response.Request.URL,
			err)
		log.Print("Trying to parse object as EU data")

		// If the previous unmarshalling fails try agin with a different schema.
		err := json.Unmarshal(respBody, &dataEuro)
		if err != nil {
			log.Printf("Error while decoding European BG data for item %s: %s",
				response.Request.URL,
				err)
			log.Print("Giving up on trying to parse BG data")
			return
		}
		log.Printf("European price: %s", dataEuro.Price)
		price, err := strconv.ParseFloat(dataEuro.Price, 64)
		if err != nil {
			log.Printf("Error while decoding European BG price for item %s: %s",
				response.Request.URL,
				err)
			return
		}

		// Update the price in the proper object.
		data.Message = dataEuro.Message
		data.Price = price
	}

	snap.Availability = data.Message
	snap.Price = data.Price
}

func (snap *Snapshot) getEbayStatus(response *http.Response) {
	ebayItem := EbayItem{}

	err := json.NewDecoder(response.Body).Decode(&ebayItem)
	if err != nil {
		log.Printf("Error while converting the Ebay JSON %s", err)
		snap.OnEbay = UnkEbay
		return
	}

	log.Printf("The status of item %s is %s", ebayItem.ID, ebayItem.Status)

	if ebayItem.Status == "Active" {
		snap.OnEbay = OnEbay
	} else {
		snap.OnEbay = OffEbay
	}
}
