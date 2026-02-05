import os
import glob 
from dotenv import load_dotenv
from slack_sdk import WebClient
from slack_sdk.errors import SlackApiError

load_dotenv()

token = os.getenv("SLACK_BOT_TOKEN")
channel_id = os.getenv("CHANNEL_ID")

image_folder = "images"

search_pattern = os.path.join(image_folder, "*.png") 
list_of_files = glob.glob(search_pattern) 

if not list_of_files:
    print(f"Помилка: У папці '{image_folder}' не знайдено жодного PNG файлу!")
    exit()
    
latest_file = max(list_of_files, key=os.path.getmtime)

print(f"✅ Знайдено найновіше зображення: {latest_file}")

if not token:
    print("Помилка: Токен не знайдено.")
else:
    client = WebClient(token=token)

try:
    response = client.files_upload_v2(
        channel=channel_id,
        file=latest_file, 
        title="Свіжий звіт",
        initial_comment=f"Ось найактуальніший дашборд ({os.path.basename(latest_file)})"
    )
    print("Зображення успішно надіслано!")

except SlackApiError as e:
    print(f"Error uploading file: {e.response['error']}")
except FileNotFoundError:
    print(f"Файл {latest_file} кудись зник під час роботи скрипта.")
