console.log("custom js loaded!");

// Function to extract JWT token from cookie
function extractJwtToken(): string | null {
    const getCookie = (name: string): string | undefined => {
        const value: string = `; ${document.cookie}`;
        const parts: string[] = value.split(`; ${name}=`);
        if (parts.length === 2) return parts.pop()?.split(';').shift();
    };

    const authCookie: string | undefined = getCookie('AuthData');
    if (!authCookie) {
        console.log("Authentication token not found");
        return null;
    }

    try {
        const decoded: string = decodeURIComponent(authCookie);
        const jwtMatch: RegExpMatchArray | null = decoded.match(/eyJ[a-zA-Z0-9_-]+\.[a-zA-Z0-9_-]+\.[a-zA-Z0-9_-]+/);
        if (jwtMatch) {
            const jwtToken: string = jwtMatch[0];
            console.log("Parsed Object:", `AuthData { jToken : "Just \"${jwtToken}\"" }`);
            console.log("JWT Token:", jwtToken);
            return jwtToken;
        } else {
            console.log("JWT token not found.");
            return null;
        }
    } catch (e) {
        console.log("Error while decoding JWT token:", e);
        return null;
    }
}

// Function to update character count
function updateCharCount(textarea: HTMLTextAreaElement): void {
    const maxlength: number = 250;
    const currentLen: number = textarea.value.length;
    const remainingChars: number = maxlength - currentLen;
    const charCountEle: HTMLElement | null = document.getElementById('charCount');
    const charCountDiv: HTMLElement | null = document.getElementById('charCountDiv');

    if (charCountEle && charCountDiv) {
        charCountEle.textContent = remainingChars.toString();
        if (remainingChars < 50) {
            charCountDiv.classList.remove('text-yellow-500');
            charCountDiv.classList.add('text-red-600');
        } else if (remainingChars < 100) {
            charCountDiv.classList.remove('text-green-600');
            charCountDiv.classList.add('text-yellow-600');
        }
    }
}

// Function to preview image
function previewImage(): void {
    const imageInput: HTMLInputElement | null = document.getElementById('imageInput') as HTMLInputElement;
    if (!imageInput?.files || imageInput.files.length === 0) return;

    const file: File = imageInput.files[0];
    if (!file.type.startsWith('image/')) return;

    const imagePreview: HTMLImageElement | null = document.getElementById('imagePreview') as HTMLImageElement;
    if (!imagePreview) return;

    const reader: FileReader = new FileReader();
    reader.readAsDataURL(file);
    reader.onload = function (e: ProgressEvent<FileReader>): void {
        if (e.target?.result) {
            imagePreview.src = e.target.result as string;
            imagePreview.style.display = 'block';
        }
    };
}

// Function to upload image
async function uploadImage(): Promise<void> {
    console.log("uploading image");
    const imageInput: HTMLInputElement | null = document.getElementById('imageInput') as HTMLInputElement;
    const statusMessage: HTMLElement | null = document.getElementById('statusMessage');

    if (!imageInput?.files || imageInput.files.length === 0) {
        if (statusMessage) statusMessage.textContent = "Please select an image file";
        return;
    }

    const file: File = imageInput.files[0];
    if (!file.type.startsWith('image/')) {
        if (statusMessage) statusMessage.textContent = "Please select an image file";
        return;
    }

    if (statusMessage) statusMessage.textContent = "Processing image...";

    const jwtToken: string | null = extractJwtToken();
    if (!jwtToken) {
        if (statusMessage) statusMessage.textContent = "Authentication token not found";
        return;
    }

    const formData: FormData = new FormData();
    formData.append('pic', file);
    if (statusMessage) statusMessage.textContent = "Uploading image...";

    try {
        const response: Response = await fetch('http://localhost:8085/api/v1/user/profile/update-image', {
            method: 'PUT',
            headers: {
                'Authorization': `Bearer ${jwtToken}`,
            },
            body: formData
        });

        if (!response.ok) {
            throw new Error(`Upload failed with status: ${response.status}`);
        }

        if (statusMessage) statusMessage.textContent = "Image uploaded successfully";
        location.reload();
    } catch (error: any) {
        if (statusMessage) statusMessage.textContent = "Error: " + error.message;
        console.error("Upload error:", error);
    }
}

// Function to create a thread
async function createThread(): Promise<void> {
    console.log("inside createThread()");
    const statusMessage: HTMLElement | null = document.getElementById('statusMessage');

    // Get field values by ID
    const communityId: string = (document.getElementById('threadCommunityID') as HTMLInputElement).value;
    const title: string = (document.getElementById('threadTitle') as HTMLInputElement).value;
    const description: string = (document.getElementById('threadDescription') as HTMLTextAreaElement).value;
    const fileInput: HTMLInputElement | null = document.getElementById('threadAttachment') as HTMLInputElement;
    const file: File | undefined = fileInput?.files?.[0];

    // Create FormData object
    const formData: FormData = new FormData();
    formData.append('threadCommunityIDForCreate', communityId);
    formData.append('threadTitleForCreate', title);
    formData.append('threadDescriptionForCreate', description);
    if (file) {
        formData.append('threadAttachment', file);
    }
    console.log("created formData ", formData);

    // Extract JWT token
    const jwtToken: string | null = extractJwtToken();
    if (!jwtToken) {
        if (statusMessage) statusMessage.textContent = "Authentication token not found";
        return;
    }

    try {
        if (statusMessage) statusMessage.textContent = "Creating thread...";
        const response: Response = await fetch('http://localhost:8085/api/v1/user/thread/create', {
            method: 'POST',
            headers: {
                'Authorization': `Bearer ${jwtToken}`,
            },
            body: formData
        });
        console.log("got response", response);

        if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status}`);
        }

        const result: any = await response.json();
        console.log("Thread created successfully:", result);

        if (statusMessage) statusMessage.textContent = "Thread created successfully! Returning to home...";
        window.location.href = "/"; // Redirect to home page
    } catch (error: any) {
        if (statusMessage) statusMessage.textContent = "Error creating thread: " + error.message;
        console.error('Error creating thread:', error);
    }
}

async function updateThread(tId : number): Promise<void> {
    console.log("inside updateThread()");
    const statusMessage: HTMLElement | null = document.getElementById('statusMessage');

    // Get field values by ID
    const communityId: string = (document.getElementById('threadCommunityID') as HTMLInputElement).value;
    const title: string = (document.getElementById('threadTitle') as HTMLInputElement).value;
    const description: string = (document.getElementById('threadDescription') as HTMLTextAreaElement).value;

    // Create FormData object
    const formData: FormData = new FormData();
    formData.append('threadCommunityIDForUpdate', communityId);
    formData.append('threadTitleForUpdate', title);
    formData.append('threadDescriptionForUpdate', description);
    formData.append("threadIDForUpdate",tId.toString())
    console.log("created formData ", formData);

    // Extract JWT token
    const jwtToken: string | null = extractJwtToken();
    if (!jwtToken) {
        if (statusMessage) statusMessage.textContent = "Authentication token not found";
        return;
    }

    try {
        if (statusMessage) statusMessage.textContent = "Creating thread...";
        const response: Response = await fetch('http://localhost:8085/api/v1/user/thread/update', {
            method: 'PUT',
            headers: {
                'Authorization': `Bearer ${jwtToken}`,
            },
            body: formData
        });
        console.log("got response", response);

        if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status}`);
        }

        const result: any = await response.json();
        console.log("Thread created successfully:", result);

        if (statusMessage) statusMessage.textContent = "Thread Updated successfully! Returning to home...";
        window.location.href = "/"; // Redirect to home page
    } catch (error: any) {
        if (statusMessage) statusMessage.textContent = "Error creating thread: " + error.message;
        console.error('Error creating thread:', error);
    }
}

function cancelForm() {
    window.location.href = '/';
}

async function downloadAttachment (threadId : number, attachmentName: string) {
    console.log("downloading attachment")
    const apiUrl = `http://localhost:8085/api/v1/thread/attachment/${threadId}`
    try {
        const response = await fetch(apiUrl,{
            method : 'GET'
        });
        if (!response.ok)
            throw new Error("Network response was not ok")
        const blob = await response.blob();
        const url = window.URL.createObjectURL(blob);

        const a = document.createElement('a');
        a.href = url;
        a.download = attachmentName;
        document.body.appendChild(a);
        a.click();

        window.URL.revokeObjectURL(url);
        document.body.removeChild(a);
    }catch (err) {
        console.log("Error while downloading: ",err)
        return;
    }
 }
