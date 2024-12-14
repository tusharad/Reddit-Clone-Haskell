'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';

interface CreateCommentModalProps {
    closeModal: () => void;
    setNewCommentAdded: (newCommentAdded: boolean) => void;
    threadId: number;
    replyCommentId: number | null;
}

const CreateCommentModal: React.FC<CreateCommentModalProps> = ({closeModal,setNewCommentAdded, threadId, replyCommentId}) => {
    const [comment, setComment] = useState('');
    const [error, setError] = useState('');
    const router = useRouter();

    const handleSubmit = async (event: React.FormEvent<HTMLFormElement>) => {
        event.preventDefault();
        setError('');
    
        const token = localStorage.getItem('jwt_token');
        if (!token) {
          setError('User is not authenticated');
          return;
        }
    
        try {
          const response = await fetch('http://localhost:8085/api/v1/user/comment/create', {
            method: 'POST',
            headers: {
              'Content-Type': 'application/json',
              Authorization: `Bearer ${token}`,
            },
            body: JSON.stringify({
              threadIDForCommentCreate: parseInt(threadId, 10),
              commentContentForCreate: comment,
              parentCommentID : replyCommentId
            }),
          });
    
          if (response.ok) {
            closeModal();
            setNewCommentAdded(true);
            router.push(`/view-thread/${threadId}`);
          } else {
            const res = await response.json();
            setError(res.message || 'Failed to create thread');
          }
        } catch (error) {
          setError('An error occurred: ' + error);
        }
      };

    return (
        <div className="fixed inset-0 bg-black bg-opacity-50 flex justify-center items-center">
          <div className="bg-white p-8 rounded-lg shadow-lg max-w-md w-full">
            <h2 className="text-2xl font-bold mb-4">Create a New Comment</h2>
            {error && <p className="text-red-500 mb-4">{error}</p>}
            <form onSubmit={handleSubmit}>
              <div className="mb-4">
                <label htmlFor="comment_" className="block text-gray-700">
                  Comment
                </label>
                <textarea
                  id="comment_"
                  value={comment}
                  onChange={(e) => setComment(e.target.value)}
                  className="w-full px-3 py-2 border rounded"
                  rows={4}
                  required
                />
              </div>
              <div className="flex justify-end space-x-2">
                <button
                  type="button"
                  onClick={closeModal}
                  className="px-4 py-2 bg-gray-600 text-white rounded hover:bg-gray-500"
                >
                  Cancel
                </button>
                <button
                  type="submit"
                  className="px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-500"
                >
                  Submit
                </button>
              </div>
            </form>
          </div>
        </div>
      );
    };

export default CreateCommentModal;