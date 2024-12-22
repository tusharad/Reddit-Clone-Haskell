'use client'

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';

interface CreateThreadModalProps {
  closeModal: () => void;
  setNewThreadAdded: (newThreadAdded: boolean) => void;
}

const CreateThreadModal: React.FC<CreateThreadModalProps> = ({closeModal,setNewThreadAdded}) => {
  const [title, setTitle] = useState('');
  const [description, setDescription] = useState('');
  const [selectedCommunity, setSelectedCommunity] = useState<number | null>(null);
  const [error, setError] = useState('');
  const [communities, setCommunities] = useState<any[]>([]);
  const router = useRouter();

  useEffect(() => {
    const fetchData = async () => {
      const communitiesRes = await fetch(`http://localhost:8085/api/v1/community`);
      const communitiesData = await communitiesRes.json();
      setCommunities(communitiesData.communities);
      if(communities.length > 0)
        setSelectedCommunity(communities[0])
    };
    fetchData();
  },[])

  const handleSubmit = async (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    setError('');

    if (!selectedCommunity) {
      setError('Please select a community');
      return;
    }

    const token = localStorage.getItem('jwt_token');
    if (!token) {
      setError('User is not authenticated');
      return;
    }

    try {
      const response = await fetch('http://localhost:8085/api/v1/user/thread/create', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          Authorization: `Bearer ${token}`,
        },
        body: JSON.stringify({
          threadTitleForCreate: title,
          threadDescriptionForCreate: description !== "" ? description : null,
          threadCommunityIDForCreate: selectedCommunity,
        }),
      });

      if (response.ok) {
        closeModal();
        setNewThreadAdded(true);
        router.push('/');
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
        <h2 className="text-2xl font-bold mb-4">Create a New Thread</h2>
        {error && <p className="text-red-500 mb-4">{error}</p>}
        <form onSubmit={handleSubmit}>
          <div className="mb-4">
            <label htmlFor="community" className="block text-gray-700">
              Select Community
            </label>
            <select
              id="community"
              value={selectedCommunity || ''}
              onChange={(e) => setSelectedCommunity(Number(e.target.value))}
              className="w-full px-3 py-2 border rounded"
              required
            >
              <option value="" disabled>
                --Select a community--
              </option>
              {communities.map((community) => (
                <option key={community.communityID} value={community.communityID}>
                  {community.communityName}
                </option>
              ))}
            </select>
          </div>
          <div className="mb-4">
            <label htmlFor="title" className="block text-gray-700">
              Thread Title
            </label>
            <input
              type="text"
              id="title"
              value={title}
              onChange={(e) => setTitle(e.target.value)}
              className="w-full px-3 py-2 border rounded"
              required
            />
          </div>
          <div className="mb-4">
            <label htmlFor="description" className="block text-gray-700">
              Description (optional)
            </label>
            <textarea
              id="description"
              value={description}
              onChange={(e) => setDescription(e.target.value)}
              className="w-full px-3 py-2 border rounded"
              rows={4}
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

export default CreateThreadModal;