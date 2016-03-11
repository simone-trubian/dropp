from time import sleep
from json import load
from boto import ses
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
    credentials_file = argv[3]

    url_list = get_urls(url_file_path)
    availability_list = []
    with open(credentials_file, 'r') as file:
        creds = load(file)

    # Open a connection to AWS.
    conn = ses.connect_to_region(
        'eu-west-1',
        aws_access_key_id=creds['ACCESS_KEY_ID'],
        aws_secret_access_key=creds['SECRET_ACCESS_KEY'])

    # Fetch subscribed recipients.
    subscribed_users = conn.list_verified_email_addresses()
    email_addrs = subscribed_users[
        'ListVerifiedEmailAddressesResponse'][
            'ListVerifiedEmailAddressesResult'][
                'VerifiedEmailAddresses']

    # Interrogate Bangood website.
    for url in url_list:
        availability_list.append(get_availability(url))
        sleep(3)

    # Format the website output to file.
    output_list = []
    for url, line in zip(url_list, availability_list):
        output_list.append(timestamp() + '~' + url + '~' + line + '\n\n')

    # Write to output file.
    with open(availability_file_path, 'a') as out_file:
        out_file.write('\n\n\n')
        for line in output_list:
            out_file.write(line)

    # Send emails.
    conn.send_email(
        email_addrs[0],
        timestamp() + ' Availability',
        '\n'.join(map(str, output_list)),
        email_addrs)
