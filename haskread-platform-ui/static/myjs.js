console.log("custom js loaded!");

function updateCharCount(textarea) {
    const maxlength = 250;
    const currentLen = textarea.value.length;
    const remainingChars = maxlength - currentLen;
    const charCountEle = document.getElementById('charCount');
    const charCountDiv = document.getElementById('charCountDiv');
    charCountEle.textContent = remainingChars;
    if (remainingChars < 50) {
        charCountDiv.classList.remove('text-yellow-500');
        charCountDiv.classList.add('text-red-600');
    }
    else if (remainingChars < 100) {
        charCountDiv.classList.remove('text-green-600');
        charCountDiv.classList.add('text-yellow-600');
    }
}

function previewImage() {
    const imageInput = document.getElementById('imageInput');
    if (!imageInput.files || imageInput.files.length === 0)
        return;
    const file = imageInput.files[0];
    if (!file.type.startsWith('image/'))
        return;
    const imagePreview = document.getElementById('imagePreview');
    const reader = new FileReader();
    reader.readAsDataURL(file);
    reader.onload = function (e) {
        imagePreview.src = e.target.result;
        imagePreview.style.display = 'block';
    };
}


async function uploadImage() {
    console.log("uploading image");
    const imageInput = document.getElementById('imageInput');

    const statusMessage = document.getElementById('statusMessage');
    try {
        if (!imageInput.files || imageInput.files.length === 0) {
            statusMessage.textContent = "Please select an image file";
            return;
        }
        const file = imageInput.files[0];
        if (!file.type.startsWith('image/')) {
            statusMessage.textContent = "Please select an image file";
            return;
        }
        statusMessage.textContent = "Processing image...";
        const getCookie = (name) => {
            const value = `; ${document.cookie}`;
            const parts = value.split(`; ${name}=`);
            if (parts.length === 2) return parts.pop().split(';').shift();
        };

        const authCookie = getCookie('AuthData');
        if (!authCookie) {
            statusMessage.textContent = "Authentication token not found";
            return;
        }

        let jwtToken;
        try {
            const decoded = decodeURIComponent(authCookie);
            const jwtMatch = decoded.match(/eyJ[a-zA-Z0-9_-]+\.[a-zA-Z0-9_-]+\.[a-zA-Z0-9_-]+/);
            if (jwtMatch) {
                const jwtToken_ = jwtMatch[0];
                console.log("Parsed Object:", `AuthData { jToken : "Just \"${jwtToken_}\"" }`);
                console.log("JWT Token:", jwtToken_);
                jwtToken = jwtToken_
            } else {
                console.log("JWT token not found.");
            }
        } catch (e) {
            statusMessage.textContent = "Error parsing authentication token";
            return;
        }
        const formData = new FormData();
        formData.append('pic', file);
        statusMessage.textContent = "Uploading image...";
        const response = await fetch('http://localhost:8085/api/v1/user/profile/update-image', {
            method: 'PUT',
            headers: {
                'Authorization': `Bearer ${jwtToken}`,
            },
            body: formData
        });

        if (!response.ok) {
            throw new Error(`Upload failed with status: ${response.status}`);
        }
        statusMessage.textContent = "Image uploaded successfully";
        location.reload();

    } catch (error) {
        statusMessage.textContent = "Error: " + error.message;
        console.error("Upload error:", error);
    }
}
