'use client';

import React, { useState, useEffect } from 'react';
import Header from '../../components/Header';
import CommunityList from '../../components/CommunityList';
import { faComment, faShare, faThumbsDown } from '@fortawesome/free-solid-svg-icons'
import { faThumbsUp as f2 } from '@fortawesome/free-regular-svg-icons'
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'


interface Comment {
  mainComment: {
    commentIDForCommentInfo: number;
    commentContentForCommentInfo: string;
    createdAtForCommentInfo: string;
    userNameForCommentInfo: string;
  };
  children: Comment[];
}

interface Community {
  communityID: number;
  communityName: string;
}

const ViewThread: React.FC<{ params: { id: number } }> = ({ params }) => {
  const thread_id = params.id;
  const [thread, setThread] = useState<any>(null);
  const [comments, setComments] = useState<Comment[]>([]);
  const [isLoggedIn, setIsLoggedIn] = useState(false);
  const [communities, setCommunities] = useState<Community[]>([]);


  useEffect(() => {

    const fetchThreadData = async () => {
      try {
        if (thread_id) {
          const threadRes = await fetch(`http://localhost:8085/api/v1/thread/${thread_id}`);
          const threadData = await threadRes.json();
          setThread(threadData);

          const commentsRes = await fetch(`http://localhost:8085/api/v1/thread/comment/${thread_id}`);
          const commentsData = await commentsRes.json();
          setComments(commentsData.comments);

          const communitiesRes = await fetch('http://localhost:8085/api/v1/community');
          const communitiesData = await communitiesRes.json();
          setCommunities(communitiesData.communities);
        }
      } catch (error) {
        console.error("Error fetching data:", error);
      }
    };

    fetchThreadData();
  }, [thread_id]);

  const renderComments = (comments: Comment[]) => {
    return comments.map((comment) => (
      <div key={comment.mainComment.commentIDForCommentInfo} className="ml-4 border-l-2 border-gray-300 pl-4">
        <div className="border p-4 mb-2 rounded shadow-sm bg-white card-bg shadow-lg rounded-lg mb-6 overflow-hidden">
          <div className="flex justify-between">
            <span className="font-semibold text-gray-500">{comment.mainComment.userNameForCommentInfo}</span>
            <span className="text-sm text-gray-500">{new Date(comment.mainComment.createdAtForCommentInfo).toLocaleDateString()}</span>
          </div>
          <p className="mt-2 text-gray-900">{comment.mainComment.commentContentForCommentInfo}</p>
          <div className="flex mt-2 text-sm space-x-2">
            <button className="hover:text-blue-500">Reply</button>
            <button className="hover:text-green-500"><FontAwesomeIcon icon={ f2 } />  0</button>
            <button className="hover:text-red-500"><FontAwesomeIcon icon={faThumbsDown} /> 0</button>
          </div>
        </div>
        {comment.children.length > 0 && renderComments(comment.children)}
      </div>
    ));
  };

  return (
    <div>
      <Header setIsLoggedIn={setIsLoggedIn} />
      <main className="container mx-auto p-4 flex">
        <div className="w-3/4">
          {thread && (
            <div className="card-bg shadow-lg rounded-lg mb-6 overflow-hidden">
              <div className="flex justify-between items-center p-4 border-b">
              <h1 className="text-3xl font-bold text-gray-900">{thread.title}</h1>
              <div className="text-sm text-right text-gray-500">
                <p>
                        Community: <span className="font-semibold">{thread.communityNameForThreadInfo}</span>
                    </p>
                    <p>
                        Created by <span className="font-semibold">{thread.userNameForThreadInfo}</span>
                    </p>
                    <p>{new Date(thread.createdAtForThreadInfo).toLocaleDateString()}</p>
              </div>
              </div>

              {thread.description &&
                (
                  <><div className="p-4"><p className="">{thread.description}</p></div></>)
              }
              <div className="flex justify-between items-center p-4 border-t">
                <div className="flex space-x-2 items-center">
                    <button className="flex items-center space-x-1 hover:text-blue-500">
                        <FontAwesomeIcon icon={ f2 } />
                        <span>{thread.upvoteCount || 0}</span>
                    </button>
                    <button className="flex items-center space-x-1 hover:text-red-500">
                        <FontAwesomeIcon icon={faThumbsDown} />
                        <span>{thread.downvoteCount || 0}</span>
                    </button>
                    <span className="flex items-center space-x-1 ">
                        <FontAwesomeIcon icon={faComment} />
                        <span>{thread.commentCount || 0}</span>
                    </span>
                </div>
                <button className="flex items-center space-x-1 text-gray-600 hover:text-green-500">
                    <FontAwesomeIcon icon={faShare} />
                    <span>Share</span>
                </button>
            </div>
            </div>

          )}

          <div className="mt-6">
          <button
              className={`mt-4 px-4 py-2 rounded bg-green-500 text-white ${!isLoggedIn ? 'opacity-50 cursor-not-allowed' : 'hover:bg-green-600'}`}
              disabled={!isLoggedIn}
            >
              Add Comment
            </button>
            <h2 className="text-2xl font-bold mb-4 text-gray-900">Comments</h2>
            {renderComments(comments)}

          
          </div>

        </div>

        <aside className="w-1/4 pl-4">
          <CommunityList communities={communities} />
        </aside>
      </main>
    </div>
  );
};

export default ViewThread;
