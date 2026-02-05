import os
from dotenv import load_dotenv
from slack_sdk import WebClient
from slack_sdk.errors import SlackApiError

load_dotenv()

token = os.getenv("SLACK_BOT_TOKEN")
channel_id = os.getenv("CHANNEL_ID")


if not token:
    print("Помилка: Токен не знайдено. Перевірте файл .env")
else:
    client = WebClient(token=token)
    
try:
  client.files_upload_v2(
    channel = channel_id,
    file = "Rplot.png",
    title = "Нова інформація по метрикам!!",
    initial_comment="Текст повідомлення, який піде разом з картинкою"
)
except SlackApiError as e:
  print(f"Error uploading file: {e.response['error']}")
else:
  print("Зображення надіслано!")
  
