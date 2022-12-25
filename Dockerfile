FROM python:alpine

COPY ./requirements.txt /app/requirements.txt

WORKDIR /app

RUN pip install -r requirements.txt

COPY *.py .
COPY static ./static
COPY templates ./templates

ENTRYPOINT [ "python" ]

CMD [ "server.py" ]
