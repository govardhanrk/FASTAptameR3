## FASTAptameR3 — Docker Quick Start Guide (Windows & macOS)

This guide is to explains how to install Docker Desktop, pull and run the FASTAptameR3 Docker image, and open the app in your browser. Screenshots referenced in this guide correspond to the images you may see in Docker Hub and Docker Desktop.

If you run into trouble, see the Troubleshooting section at the end.

---

### 1) What is Docker and why are we using it?

- **Docker** lets us package software and all of its dependencies into a single container. This avoids installation hassles and version conflicts on your computer.
- With Docker, you can run the FASTAptameR3 app without installing R or any libraries.

---

### 2) System requirements

- **Administrative rights** to install applications
- **Stable internet connection** (to download Docker Desktop and the FASTAptameR3 image)
- Disk space: ~2–5 GB free
- Windows and macOS specifics are below

---

### 3) Install Docker Desktop

#### A. Windows 10/11

1. Go to the Docker Desktop download page: `https://www.docker.com/products/docker-desktop/` and download the Windows installer.
2. Run the installer. Keep the defaults. If asked to enable **WSL 2** and restart, accept it. The installer will:
   - Enable the Windows Subsystem for Linux (WSL 2)
   - Install the Linux kernel update package if needed
   - Ask you to restart your computer
3. After restart, open **Docker Desktop**. If prompted, sign in or create a free Docker Hub account.
4. Confirm Docker is running: the whale icon in the system tray should show “Docker Desktop is running.”

Notes for Windows:
- If you are on Windows Home or some enterprise-managed devices, WSL 2 is the recommended backend and usually installs automatically from Docker Desktop. If it does not, see Microsoft’s WSL docs: `https://aka.ms/wslinstall`.

#### B. macOS (Intel or Apple Silicon: M1/M2/M3)

1. Go to `https://www.docker.com/products/docker-desktop/`.
2. Download the correct installer for your Mac:
   - **Apple silicon (M1/M2/M3)**: choose the Apple chip build
   - **Intel**: choose the Intel build
3. Open the `.dmg`, drag Docker Desktop to Applications, then open it.
4. Grant permissions if macOS asks. Sign in or create a free Docker Hub account.
5. Wait for the whale icon to show “Docker Desktop is running.”

Notes for macOS:
- On first launch, macOS may ask for networking permissions. Approve them.
- On older Macs, Docker may ask to install Rosetta (for Apple silicon). Approve if prompted.

---

### 4) Find the FASTAptameR3 image in Docker Hub

You can pull and run the published image directly. The repository is:

- **Docker Hub repo:** `yongfangqin/fastaptamer3`
- **Latest tag:** `latest`

There are two easy ways to get it:

#### Option A: Use Docker Desktop (simple, point‑and‑click)
1. Open Docker Desktop.
2. In the left sidebar, select **Images**.
3. Click **Pull** (or use the search bar at the top of Docker Desktop) and search for `yongfangqin/fastaptamer3`.
4. Select the image and click **Pull**. Wait for the download to finish.

![Searching and pulling the image in Docker Desktop](docker-guide-images/docker_image_search.png)

Caption: Use the search bar for `yongfangqin/fastaptamer3` and pull it.

#### Option B: Use a one‑line command (optional)
If you are comfortable with a terminal/PowerShell:

```bash
docker pull yongfangqin/fastaptamer3:latest
```

---

### 5) Run the FASTAptameR3 container

FASTAptameR3 is a Shiny app that listens on container port **3838**. You need to map it to a port on your computer and then open it in a browser.

You can run it from the Docker Desktop GUI or with a command.

#### Option A: Run from Docker Desktop (recommended for non‑technical users)
1. Open Docker Desktop and go to **Images**.
2. Find `yongfangqin/fastaptamer3` and click **Run**.
3. In the Run dialog:
   - Expand **Optional settings** if it’s collapsed.
   - Under **Ports**, set **Host port** to `3838` and ensure the container port shows `:3838/tcp`.
     - If 3838 is already used on your computer, pick another free port such as `3839`.
   - Click **Run**.

![Run dialog showing port mapping 3838 -> 3838](docker-guide-images/docker_run_container.png)

Caption: Set the Host port to 3838 (or another free port) and start the container.
4. After the container starts, it should appear under **Containers**. Wait until its status is “Running.”

#### Option B: Run with a command (optional)

```bash
docker run --rm -p 3838:3838 yongfangqin/fastaptamer3:latest
```

Explanation:
- `-p 3838:3838` maps your computer’s port 3838 to the app’s port 3838.
- `--rm` cleans up the container when you stop it.

```bash
docker run --rm -p 3838:3838 yongfangqin/fastaptamer3:latest
```

---

### 6) Using the app

1. With the container running, open your browser to `http://localhost:3838`
   - If you used another port (e.g., 3839): `http://localhost:3839`
2. Interact with the FASTAptameR3 interface as you normally would.

---

### 7) Start/stop, update, and remove

- **Stop the app**
  - From Docker Desktop: go to **Containers**, click the square stop button for the running container.
  - From terminal: press `Ctrl + C` in the window where it’s running; or run `docker stop <container_name>`.

- **Update to the newest image**
  - Stop any running containers of FASTAptameR3.
  - Pull again: `docker pull yongfangqin/fastaptamer3:latest`
  - Run a new container; the old one can be removed.

- **Remove old containers/images** (optional housekeeping)
  - In Docker Desktop, use the **Delete** button on stopped containers or unused images.

---

### 8) Troubleshooting

- **Port already in use**
  - Symptom: Browser shows an error or Docker refuses to start with `-p 3838:3838`.
  - Fix: Choose a different host port (e.g., 3839) in the Run dialog or command: `-p 3839:3838`, then open `http://localhost:3839`.

- **Docker Desktop not running**
  - Symptom: Pulls/starts fail, or the whale icon shows an error.
  - Fix: Open Docker Desktop and wait until it says “Docker Desktop is running.” Restart the app or your computer if necessary.

- **Insufficient resources (memory/CPU)**
  - Symptom: App feels slow or crashes.
  - Fix: In Docker Desktop, go to **Settings → Resources** and increase CPU/Memory if possible.

- **Corporate/University network restrictions**
  - Symptom: Pull fails or is very slow.
  - Fix: Try another network (home/hotspot) or contact IT for Docker Hub access.

- **Apple silicon (M‑series) compatibility**
  - Docker Desktop supports Apple chips. If you see an architecture warning, ensure you downloaded the correct Docker Desktop build for Apple silicon. The `yongfangqin/fastaptamer3` image should run on current versions of Docker Desktop; if needed, pull the latest image.

---

### 9) Where to get help

- If something doesn’t work, please share:
  - Your OS (Windows/macOS), version
  - Docker Desktop version
  - What you tried and what you saw (screenshots help)

---

This document is intended to be comprehensive yet beginner‑friendly. If you find a step confusing, please let us know so we can improve it.


