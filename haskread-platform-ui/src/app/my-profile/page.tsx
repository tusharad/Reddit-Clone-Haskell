'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import Header from '../components/Header';
import ThreadCard from '../components/ThreadCard';
import Link from 'next/link';

const Profile: React.FC = () => {
  const [userData, setUserData] = useState<any>(null);
  const [userThreads, setUserThreads] = useState<any[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const router = useRouter();

  useEffect(() => {
    console.log("made it till here")
    const token = localStorage.getItem('jwt_token');

    if (!token) {
      console.log("token not found")
      router.push('/');
      return;
    }

    const fetchProfile = async () => {
      try {
        const userProfileRes = await fetch('http://localhost:8085/api/v1/user/profile', {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        });

        if (!userProfileRes.ok) {
          router.push('/');
          return;
        }

        const userProfileData = await userProfileRes.json();
        setUserData(userProfileData);

        const userThreadsRes = await fetch(
          `http://localhost:8085/api/v1/thread/all?limit=100&userId=${userProfileData.userIDForUPR}`,
          {
            headers: {
              Authorization: `Bearer ${token}`,
            },
          }
        );

        const userThreadsData = await userThreadsRes.json();
        setUserThreads(userThreadsData.threads);
      } catch (error) {
        console.error('Failed to fetch profile data:', error);
      } finally {
        setIsLoading(false);
      }
    };

    fetchProfile();
  }, [router]);

  if (isLoading) {
    return <div className="flex justify-center items-center h-screen">Loading...</div>;
  }

  return (
    <div className="flex flex-col min-h-screen bg-[#F4EEFF]">
      <Header setIsLoggedIn={() => {}} />
      <main className="container mx-auto mt-16 px-6 flex-grow">
        {userData && (
          <div className="mb-6">
            <h1 className="text-3xl font-bold text-center mb-4">Profile</h1>
            <div className="bg-white shadow-lg rounded-lg p-6">
              <p><strong>Username:</strong> {userData.userNameForUPR}</p>
              <p><strong>Email:</strong> {userData.userEmail}</p>
              <p><strong>Account Created:</strong> {new Date(userData.userCreatedAt).toLocaleDateString()}</p>
              <div className="flex justify-around mt-4">
                <button className="bg-blue-600 text-white px-4 py-2 rounded hover:bg-blue-500">Change Password</button>
                <button className="bg-blue-600 text-white px-4 py-2 rounded hover:bg-blue-500">Edit Profile</button>
              </div>
            </div>
          </div>
        )}

        <div>
          <h2 className="text-2xl font-bold text-center mb-4">Your Threads</h2>
          {userThreads.length > 0 ? (
            userThreads.map((thread) => (
              <ThreadCard
                key={thread.threadIDForThreadInfo}
                title={thread.title}
                communityName={thread.communityNameForThreadInfo}
                userName={thread.userNameForThreadInfo}
                createdAt={thread.createdAtForThreadInfo}
                description={thread.description}
                upvoteCount={thread.upvoteCount}
                downvoteCount={thread.downvoteCount}
                commentCount={thread.commentCount}
                threadIDForThreadInfo={thread.threadIDForThreadInfo}
              />
            ))
          ) : (
            <p className="text-center">No threads found.</p>
          )}
        </div>
      </main>

      <footer className="footer-bg mt-auto py-4">
        <div className="container mx-auto text-center">
          <p className="text-white">
            <strong>HaskRead</strong> by <Link href="https://www.linkedin.com/in/tushar-adhatrao/" className="text-blue-300 hover:underline">Tushar Adhatrao</Link>.
            The source code is licensed
            <Link href="https://opensource.org/license/mit" className="text-blue-300 hover:underline">MIT</Link>.
          </p>
        </div>
      </footer>
    </div>
  );
};

export default Profile;