import os
import zipfile
from datetime import datetime
from garminconnect import Garmin


username = "garmin_email"
password = "garmin_pw"
download_path = "path-for-fit-files"

def sanitize_datetime_string(datetime_str):
    """Reformat datetime string to 'YYYY-MM-DD-HH-MM-SS'."""
    dt_object = datetime.strptime(datetime_str, "%Y-%m-%d %H:%M:%S")
    return dt_object.strftime("%Y-%m-%d-%H-%M-%S")

def download_latest_fit_files():
    client = Garmin(username, password)
    client.login()

    activities = client.get_activities(0, 10)  # Fetch the 10 most recent activities
    downloaded_files = set(os.listdir(download_path))  # Check existing files

    for activity in activities:
        start_time = activity["startTimeLocal"]  # Format: 'YYYY-MM-DD HH:MM:SS'
        formatted_time = sanitize_datetime_string(start_time)
        fit_filename = f"{formatted_time}.fit"
        
        if fit_filename not in downloaded_files:
            # Download the activity as a ZIP archive
            zip_data = client.download_activity(
                activity["activityId"],
                dl_fmt = client.ActivityDownloadFormat.ORIGINAL
            )
            zip_filename = os.path.join(download_path, f"temp_{activity['activityId']}.zip")

            # Save ZIP data
            with open(zip_filename, "wb") as zip_file:
                zip_file.write(zip_data)
            
            # Extract and rename the FIT file
            with zipfile.ZipFile(zip_filename, 'r') as zip_ref:
                zip_ref.extractall(download_path)
            
            # Rename the extracted FIT file
            extracted_files = [f for f in os.listdir(download_path) if f.endswith(".fit")]
            if extracted_files:
                latest_fit_file = max(extracted_files, key=lambda f: os.path.getctime(os.path.join(download_path, f)))
                os.rename(os.path.join(download_path, latest_fit_file), os.path.join(download_path, fit_filename))
            
            # Delete the ZIP file
            os.remove(zip_filename)
            print(f"Downloaded and renamed: {fit_filename}")

if __name__ == "__main__":
    download_latest_fit_files()
