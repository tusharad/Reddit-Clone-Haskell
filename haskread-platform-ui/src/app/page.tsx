'use client';

import React, { useState, useEffect } from 'react';
import Header from './components/Header';
import ThreadCard from './components/ThreadCard';
import CommunityList from './components/CommunityList';
import Link from 'next/link';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { faChartBar, faChevronDown, faArrowTrendUp, faCircle, faPersonWalking } from '@fortawesome/free-solid-svg-icons'
import { useSearchParams } from 'next/navigation'

const Home: React.FC = () => {
  const [threads, setThreads] = useState<any[]>([]);
  const [communities, setCommunities] = useState<any[]>([]);
  const [currentUserVotes, setCurrentUserVotes] = useState<any[]>([]);
  const [offset, setOffset] = useState(0);
  const [communityId, setCommunityId] = useState<number | null>(null);
  const [isLoading, setIsLoading] = useState(true);
  const [threadsCount, setThreadsCount] = useState(0);
  const [isLoggedIn, setIsLoggedIn] = useState(false);
  const [newThreadAdded, setNewThreadAdded] = useState(false);
  const searchParams = useSearchParams()
  
  useEffect(() => {
    const offsetQuery = searchParams.get('offset')
    const communityIdQuery = searchParams.get('community_id')

    if (offsetQuery && parseInt(offsetQuery)) setOffset(parseInt(offsetQuery))
    if (communityIdQuery && parseInt(communityIdQuery)) setCommunityId(parseInt(communityIdQuery))

    const fetchData = async () => {
      setIsLoading(true);
      let fetchThreadUrl = `http://localhost:8085/api/v1/thread/all?limit=10&offset=${offset}`
      if (communityId) fetchThreadUrl += `&communityId=${communityId}`      

      const threadRes = await fetch(fetchThreadUrl);
      const communitiesRes = await fetch(`http://localhost:8085/api/v1/community`);
  
      const threadData = await threadRes.json();
      const communitiesData = await communitiesRes.json();

      const token = localStorage.getItem('jwt_token');
      if (token) {
        console.log("got token", token)
        const threadIdList = threadData.threads.map((t) => {
          return t.threadIDForThreadInfo
        })
        try {
          const response = await fetch('http://localhost:8085/api/v1/user/thread_votes', {
            method: 'POST',
            headers: {
              'Content-Type': 'application/json',
              Authorization: `Bearer ${token}`,
            },
            body: JSON.stringify({
              threadListForVotes : threadIdList,
            }),
          });
    
          if (response.ok) {
            const data = await response.json();
            setCurrentUserVotes(data);
            console.log("got data: ", data);
          } else {
            const res = await response.text();
            console.log(res,'Failed to create thread');
          }
        } catch (error) {
          console.log('An error occurred: ' + error);
        }
      } else {
        console.log("could not find token")
      }

      setThreads(threadData.threads);
      setThreadsCount(threadData.threadsCount);
      setCommunities(communitiesData.communities);

      setIsLoading(false);
    };

    fetchData();
  }, [offset, communityId, newThreadAdded]);

  const handleNext = () => {
    if (threadsCount >= 10) {
      setOffset((prevOffset) => prevOffset + 10);
    }
  };

  const handlePrevious = () => {
    if (offset > 0) {
      setOffset((prevOffset) => prevOffset - 10);
    }
  };

  const getUserPostReaction = (threadId : number) => {
    const currentVotes = currentUserVotes;
    for (let i = 0; i < currentVotes.length; i++) {
      if(currentVotes[i][0] == threadId){
        if(currentVotes[i][1] == true)
          return 1;
        else
          return 2;
      }
    }
    return 0;
  }

  return (
    <div className="flex flex-col min-h-screen bg-[#F4EEFF]">
      <Header setIsLoggedIn={setIsLoggedIn} setNewThreadAdded={setNewThreadAdded} />
      <main className="container mx-auto mt-16 px-6 flex-grow">
        <div className="flex flex-wrap lg:flex-nowrap -mx-4">
          {/* Threads */}
          <div className="w-full lg:w-3/4 px-4">
            <p className="text-3xl text-center mb-6 text-gray-800">Threads</p>

            {/* Tabs */}
            <div className="flex justify-center mb-6">
              <ul className="flex space-x-4 bg-white rounded-full shadow-lg">
                {/* Dropdown Menu for Top Voted */}
                <li className="relative">
                  <button
                    id="topVotedButton"
                    onClick={() => toggleDropdown('topVotedList')}
                    className="px-4 py-2 rounded-full bg-blue-600 text-white font-semibold cursor-pointer shadow focus:outline-none"
                  >
                    <FontAwesomeIcon icon={faChartBar} /> Top Voted <FontAwesomeIcon icon={faChevronDown} />
                  </button>
                  <div id="topVotedList" className="absolute z-10 mt-2 bg-white rounded-md shadow-lg hidden">
                    <ul className="py-1">
                      <li><p className="block px-4 py-2 text-gray-800 hover:bg-blue-100">Top of Day</p></li>
                      <li><p className="block px-4 py-2 text-gray-800 hover:bg-blue-100">Top of Month</p></li>
                      <li><p className="block px-4 py-2 text-gray-800 hover:bg-blue-100">All Time</p></li>
                    </ul>
                  </div>
                </li>
                <li className="px-4 py-2 rounded-full text-gray-800 hover:bg-blue-100 cursor-pointer transition shadow">
                   <FontAwesomeIcon icon={faArrowTrendUp} /> Trending
                </li>
                <li className="px-4 py-2 rounded-full text-gray-800 hover:bg-blue-100 cursor-pointer transition shadow">
                  <FontAwesomeIcon icon={faCircle} /> New
                </li>
                <li className="px-4 py-2 rounded-full text-gray-800 hover:bg-blue-100 cursor-pointer transition shadow">
                  <FontAwesomeIcon icon={faPersonWalking} /> Following
                </li>
              </ul>
            </div>
            {/* End of Tabs */}

            {/* Post Cards */}
            {isLoading ? (
              <div className="text-center">Loading...</div>
            ) : (
              threads.map((thread) => (
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
                  userPostReaction={getUserPostReaction(thread.threadIDForThreadInfo)}
                />
              ))
            )}
            {/* Repeat Post Cards as needed */}

            {/* Pagination */}
            <nav className="flex justify-center items-center mt-8 py-2 px-2">
              <button
                onClick={handlePrevious}
                className={`px-3 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-500 mx-1 ${offset === 0 ? 'opacity-50 cursor-not-allowed' : ''}`}
                disabled={offset === 0}
              >
                Previous
              </button>
              <button
                onClick={handleNext}
                className={`px-3 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-500 mx-1 ${threadsCount < 10 ? 'opacity-50 cursor-not-allowed' : ''}`}
                disabled={threadsCount < 10}
              >
                Next page
              </button>
            </nav>
            {/* End of Pagination */}
          </div>
          {/* End of Threads */}

          {/* Sidebar */}
          <div className="w-full lg:w-1/4 px-4">
            <p className="text-2xl text-center mb-4 text-gray-800">Trending Communities</p>
            <CommunityList communities={communities} setCommunityId={setCommunityId} />
          </div>
          {/* End of Sidebar */}
        </div>
      </main>

      {/* Footer */}
      <footer className="footer-bg mt-auto py-4">
        <div className="container mx-auto text-center">
          <p className="text-white">
            <strong>HaskRead</strong> by <Link href="https://www.linkedin.com/in/tushar-adhatrao/" className="text-blue-300 hover:underline">Tushar Adhatrao</Link>.
            The source code is licensed under
            <Link href="https://opensource.org/license/mit" className="text-blue-300 hover:underline"> MIT</Link>.
          </p>
        </div>
      </footer>
      {/* End of Footer */}
    </div>
  );
};

export default Home;

// Helper function for toggling dropdowns
const toggleDropdown = (id: string) => {
  const element = document.getElementById(id);
  if (element) {
    element.classList.toggle('hidden');
  }
};
