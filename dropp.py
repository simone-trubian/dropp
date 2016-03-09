from time import sleep
from datetime import datetime
from sys import argv
from requests import get


def timestamp():
    return datetime.utcnow().isoformat()


def get_urls(url_file_path):
    """ Fetch url list from file.
    """
    url_list = []
    with open(url_file_path, 'r') as f1:
        for line in f1:
            url_list.append(line.strip())

    return url_list


def get_availability(url):
    """ Get Bangood item availability starting from a url.
    """
    response = get(url)
    if response.status_code >= 300:
        return 'error'
    else:
        body = response.content.decode('utf-8')
        if url.endswith('html'):
            tag_pos = body.find('class="status"')
            return body[tag_pos:tag_pos + 50]
        else:
            return body


if __name__ == '__main__':

    url_file_path = argv[1]
    availability_file_path = argv[2]

    url_list = get_urls(url_file_path)
    availability_list = []

    # Interrogate Bangood website.
    for url in url_list:
        availability_list.append(get_availability(url))
        sleep(3)

    # Format the website output to file.
    with open(availability_file_path, 'a') as out_file:
        out_file.write('\n\n\n')
        for url, line in zip(url_list, availability_list):
            out_file.write(timestamp() + '~' + url + '~' + line + '\n\n')
