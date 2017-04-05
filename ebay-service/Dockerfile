FROM python:3

MAINTAINER Simone Trubian "simone.trubian@gmail.com"

ADD . /

RUN pip install -r requirements.txt

ENTRYPOINT ["python", "./main.py"]

EXPOSE 9090
