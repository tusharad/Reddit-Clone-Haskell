var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g = Object.create((typeof Iterator === "function" ? Iterator : Object).prototype);
    return g.next = verb(0), g["throw"] = verb(1), g["return"] = verb(2), typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (g && (g = 0, op[0] && (_ = 0)), _) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
console.log("custom js loaded!");
// Function to extract JWT token from cookie
function extractJwtToken() {
    var getCookie = function (name) {
        var _a;
        var value = "; ".concat(document.cookie);
        var parts = value.split("; ".concat(name, "="));
        if (parts.length === 2)
            return (_a = parts.pop()) === null || _a === void 0 ? void 0 : _a.split(';').shift();
    };
    var authCookie = getCookie('AuthData');
    if (!authCookie) {
        console.log("Authentication token not found");
        return null;
    }
    try {
        var decoded = decodeURIComponent(authCookie);
        var jwtMatch = decoded.match(/eyJ[a-zA-Z0-9_-]+\.[a-zA-Z0-9_-]+\.[a-zA-Z0-9_-]+/);
        if (jwtMatch) {
            var jwtToken = jwtMatch[0];
            console.log("Parsed Object:", "AuthData { jToken : \"Just \"".concat(jwtToken, "\"\" }"));
            console.log("JWT Token:", jwtToken);
            return jwtToken;
        }
        else {
            console.log("JWT token not found.");
            return null;
        }
    }
    catch (e) {
        console.log("Error while decoding JWT token:", e);
        return null;
    }
}
// Function to update character count
function updateCharCount(textarea) {
    var maxlength = 250;
    var currentLen = textarea.value.length;
    var remainingChars = maxlength - currentLen;
    var charCountEle = document.getElementById('charCount');
    var charCountDiv = document.getElementById('charCountDiv');
    if (charCountEle && charCountDiv) {
        charCountEle.textContent = remainingChars.toString();
        if (remainingChars < 50) {
            charCountDiv.classList.remove('text-yellow-500');
            charCountDiv.classList.add('text-red-600');
        }
        else if (remainingChars < 100) {
            charCountDiv.classList.remove('text-green-600');
            charCountDiv.classList.add('text-yellow-600');
        }
    }
}
// Function to preview image
function previewImage() {
    var imageInput = document.getElementById('imageInput');
    if (!(imageInput === null || imageInput === void 0 ? void 0 : imageInput.files) || imageInput.files.length === 0)
        return;
    var file = imageInput.files[0];
    if (!file.type.startsWith('image/'))
        return;
    var imagePreview = document.getElementById('imagePreview');
    if (!imagePreview)
        return;
    var reader = new FileReader();
    reader.readAsDataURL(file);
    reader.onload = function (e) {
        var _a;
        if ((_a = e.target) === null || _a === void 0 ? void 0 : _a.result) {
            imagePreview.src = e.target.result;
            imagePreview.style.display = 'block';
        }
    };
}
// Function to upload image
function uploadImage() {
    return __awaiter(this, void 0, void 0, function () {
        var imageInput, statusMessage, file, jwtToken, formData, response, error_1;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    console.log("uploading image");
                    imageInput = document.getElementById('imageInput');
                    statusMessage = document.getElementById('statusMessage');
                    if (!(imageInput === null || imageInput === void 0 ? void 0 : imageInput.files) || imageInput.files.length === 0) {
                        if (statusMessage)
                            statusMessage.textContent = "Please select an image file";
                        return [2 /*return*/];
                    }
                    file = imageInput.files[0];
                    if (!file.type.startsWith('image/')) {
                        if (statusMessage)
                            statusMessage.textContent = "Please select an image file";
                        return [2 /*return*/];
                    }
                    if (statusMessage)
                        statusMessage.textContent = "Processing image...";
                    jwtToken = extractJwtToken();
                    if (!jwtToken) {
                        if (statusMessage)
                            statusMessage.textContent = "Authentication token not found";
                        return [2 /*return*/];
                    }
                    formData = new FormData();
                    formData.append('pic', file);
                    if (statusMessage)
                        statusMessage.textContent = "Uploading image...";
                    _a.label = 1;
                case 1:
                    _a.trys.push([1, 3, , 4]);
                    return [4 /*yield*/, fetch('http://localhost:8085/api/v1/user/profile/update-image', {
                            method: 'PUT',
                            headers: {
                                'Authorization': "Bearer ".concat(jwtToken),
                            },
                            body: formData
                        })];
                case 2:
                    response = _a.sent();
                    if (!response.ok) {
                        throw new Error("Upload failed with status: ".concat(response.status));
                    }
                    if (statusMessage)
                        statusMessage.textContent = "Image uploaded successfully";
                    location.reload();
                    return [3 /*break*/, 4];
                case 3:
                    error_1 = _a.sent();
                    if (statusMessage)
                        statusMessage.textContent = "Error: " + error_1.message;
                    console.error("Upload error:", error_1);
                    return [3 /*break*/, 4];
                case 4: return [2 /*return*/];
            }
        });
    });
}
// Function to create a thread
function createThread() {
    return __awaiter(this, void 0, void 0, function () {
        var statusMessage, communityId, title, description, fileInput, file, formData, jwtToken, response, result, error_2;
        var _a;
        return __generator(this, function (_b) {
            switch (_b.label) {
                case 0:
                    console.log("inside createThread()");
                    statusMessage = document.getElementById('statusMessage');
                    communityId = document.getElementById('threadCommunityID').value;
                    title = document.getElementById('threadTitle').value;
                    description = document.getElementById('threadDescription').value;
                    fileInput = document.getElementById('threadAttachment');
                    file = (_a = fileInput === null || fileInput === void 0 ? void 0 : fileInput.files) === null || _a === void 0 ? void 0 : _a[0];
                    formData = new FormData();
                    formData.append('threadCommunityIDForCreate', communityId);
                    formData.append('threadTitleForCreate', title);
                    formData.append('threadDescriptionForCreate', description);
                    if (file) {
                        formData.append('threadAttachment', file);
                    }
                    console.log("created formData ", formData);
                    jwtToken = extractJwtToken();
                    if (!jwtToken) {
                        if (statusMessage)
                            statusMessage.textContent = "Authentication token not found";
                        return [2 /*return*/];
                    }
                    _b.label = 1;
                case 1:
                    _b.trys.push([1, 4, , 5]);
                    if (statusMessage)
                        statusMessage.textContent = "Creating thread...";
                    return [4 /*yield*/, fetch('http://localhost:8085/api/v1/user/thread/create', {
                            method: 'POST',
                            headers: {
                                'Authorization': "Bearer ".concat(jwtToken),
                            },
                            body: formData
                        })];
                case 2:
                    response = _b.sent();
                    console.log("got response", response);
                    if (!response.ok) {
                        throw new Error("HTTP error! status: ".concat(response.status));
                    }
                    return [4 /*yield*/, response.json()];
                case 3:
                    result = _b.sent();
                    console.log("Thread created successfully:", result);
                    if (statusMessage)
                        statusMessage.textContent = "Thread created successfully! Returning to home...";
                    window.location.href = "/"; // Redirect to home page
                    return [3 /*break*/, 5];
                case 4:
                    error_2 = _b.sent();
                    if (statusMessage)
                        statusMessage.textContent = "Error creating thread: " + error_2.message;
                    console.error('Error creating thread:', error_2);
                    return [3 /*break*/, 5];
                case 5: return [2 /*return*/];
            }
        });
    });
}
function updateThread(tId) {
    return __awaiter(this, void 0, void 0, function () {
        var statusMessage, communityId, title, description, formData, jwtToken, response, result, error_3;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    console.log("inside updateThread()");
                    statusMessage = document.getElementById('statusMessage');
                    communityId = document.getElementById('threadCommunityID').value;
                    title = document.getElementById('threadTitle').value;
                    description = document.getElementById('threadDescription').value;
                    formData = new FormData();
                    formData.append('threadCommunityIDForUpdate', communityId);
                    formData.append('threadTitleForUpdate', title);
                    formData.append('threadDescriptionForUpdate', description);
                    formData.append("threadIDForUpdate", tId.toString());
                    console.log("created formData ", formData);
                    jwtToken = extractJwtToken();
                    if (!jwtToken) {
                        if (statusMessage)
                            statusMessage.textContent = "Authentication token not found";
                        return [2 /*return*/];
                    }
                    _a.label = 1;
                case 1:
                    _a.trys.push([1, 4, , 5]);
                    if (statusMessage)
                        statusMessage.textContent = "Creating thread...";
                    return [4 /*yield*/, fetch('http://localhost:8085/api/v1/user/thread/update', {
                            method: 'PUT',
                            headers: {
                                'Authorization': "Bearer ".concat(jwtToken),
                            },
                            body: formData
                        })];
                case 2:
                    response = _a.sent();
                    console.log("got response", response);
                    if (!response.ok) {
                        throw new Error("HTTP error! status: ".concat(response.status));
                    }
                    return [4 /*yield*/, response.json()];
                case 3:
                    result = _a.sent();
                    console.log("Thread created successfully:", result);
                    if (statusMessage)
                        statusMessage.textContent = "Thread Updated successfully! Returning to home...";
                    window.location.href = "/"; // Redirect to home page
                    return [3 /*break*/, 5];
                case 4:
                    error_3 = _a.sent();
                    if (statusMessage)
                        statusMessage.textContent = "Error creating thread: " + error_3.message;
                    console.error('Error creating thread:', error_3);
                    return [3 /*break*/, 5];
                case 5: return [2 /*return*/];
            }
        });
    });
}
function cancelForm() {
    window.location.href = '/';
}
